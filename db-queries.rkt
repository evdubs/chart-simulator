#lang racket/base

(require db
         db/util/datetime
         gregor
         plot
         racket/list
         "cmd-line.rkt"
         "structs.rkt")

(provide get-date-ohlc
         get-date-volume
         get-random-sp-500-symbols
         get-random-symbols-over-price)

(define dbc (postgresql-connect #:user (db-user) #:database (db-name) #:password (db-pass)))

(define (get-date-ohlc ticker-symbol start-date end-date)
  (let ([price-query (query-rows dbc "
select
  date::text,
  open,
  high,
  low,
  close
from
  iex.split_adjusted_chart(
    $1,
    case
      when $2::text::date > (select max(date) from iex.chart) then (select max(date) from iex.chart)
      else $2::text::date
    end,
    $3::text::date,
    false);
"
                                 ticker-symbol
                                 start-date
                                 end-date)])
    (map (λ (row) (dohlc (->posix (iso8601->date (vector-ref row 0)))
                         (vector-ref row 1) (vector-ref row 2) (vector-ref row 3) (vector-ref row 4)))
         price-query)))

(define (get-date-volume ticker-symbol start-date end-date)
  (let ([volume-query (query-rows dbc "
select
  date::text,
  volume
from
  iex.split_adjusted_chart(
    $1,
    case
      when $2::text::date > (select max(date) from iex.chart) then (select max(date) from iex.chart)
      else $2::text::date
    end,
    $3::text::date,
    false);
"
                                  ticker-symbol
                                  start-date
                                  end-date)])
    (map (λ (r) (dv (ivl (- (->posix (iso8601->date (vector-ref r 0))) 43200)
                         (+ (->posix (iso8601->date (vector-ref r 0))) 43200))
                    (ivl 70 (+ (vector-ref r 1) 70))))
         volume-query)))

(define (get-random-sp-500-symbols count)
  (let ([random-symbol-query (query-rows dbc "
select
  c.act_symbol
from
  quandl.wiki_price c
join
  nasdaq.symbol s
on
  c.act_symbol = s.act_symbol and 
  c.date = '2000-01-04' and
  s.last_seen = (select max(last_seen) from nasdaq.symbol) and
  s.is_etf = false and
  s.is_test_issue = false and
  s.is_next_shares = false and
  s.security_name !~ 'ETN' and
  s.nasdaq_symbol !~ '[-\\$\\+\\*#!@%\\^=~]' and
  case when s.nasdaq_symbol ~ '[A-Z]{4}[L-Z]'
    then s.security_name !~ '(Note|Preferred|Right|Unit|Warrant)'
    else true
  end
join
  spdr.etf_holding e
on
  c.act_symbol = e.component_symbol and
  e.etf_symbol = 'SPY' and
  e.date = (select max(date) from spdr.etf_holding where etf_symbol = 'SPY')
order by
  random()
limit $1;
"
                                         count)])
    (flatten (map (λ (v) (vector->list v)) random-symbol-query))))

(define (get-random-symbols-over-price price count)
  (let ([q (query-rows dbc "
select
  w.act_symbol
from
  quandl.wiki_price w
join
  (select
    act_symbol,
    mul(new_share_amount) / mul(old_share_amount) as split_ratio
  from
    yahoo.stock_split
  where
    date >= '2000-01-01' and
    new_share_amount != 0 and
    old_share_amount != 0
  group by
    act_symbol
  order by
    act_symbol) s
on
  w.act_symbol = s.act_symbol
where
  w.date = '2000-01-03' and
  w.close / s.split_ratio > $1
order by
  random()
limit $2;
"
                       price
                       count)])
    (flatten (map (λ (v) (vector->list v)) q))))
