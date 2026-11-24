+++
title = "Simulation and Plotting with fincl"
author = ["Aatmun Baxi"]
date = 2026-11-24
tags = ["lisp"]
draft = false
weight = 2002
section = "Computing"
+++

<div class="tldr">

Simulation is a new major feature added to fincl, a quantitative finance library
for common lisp. We briefly describe the why, what, and how of the `simulate`
macro, demonstrating its use with a classical delta-hedging example. A
visualization of simulation results is provided with fincl-viz.

</div>

Some time back I wrote about [fincl](https://github.com/aatmunbaxi/fincl), a quantitative finance library in common lisp that
I am working on. Progress on the library itself continues, with a new major
feature landing: simulation of scenarios and statistics on synthetically
generated data. Alongside this simulation feature is the ability to plot
information about these simulations with `plotly`. The result is a nice sandbox
with which users can test trading strategies and evolution of scenarios on
generated data.


## Simulation {#simulation}

Frequently, one wants to design a financial strategy or process that will be
executed as time passes. To test the strategy, one often runs this strategy on
known historical data, called _backtesting_. There are many tools out there that
can backtest trading strategies, so fincl does not attempt to add to the pile.
Instead it allows simulation over many possible futures, driven by its suite of
stochastic processes. Aggregated statistics from the simulation can be tracked
and summarized upon completion.

The general loop of simulating such scenarios follows a predictable pattern. For
each simulated path, we must initialize local variables of interest such as a
portfolio, step forward in time while updating our local variables at each step,
then aggregate summary statistics for values recorded within each path and final
values reached at the end of each path.

```text
For each possible future path:


  +------------------------------------+
  |        initialize local vars       |
  | (e.g. initial portfolio positions) |
  +------------------+-----------------+
                     |
  +- loop -----------|-----------------+
  |                  v                 |
  |       +--------------------+       |
  |  +--->| step path forward  |       |
  |  |    +---------+----------+       |
  |  |              |                  |
  |  |              v                  |
  |  |  +-----------------------+      |
  |  +--| update/record local   |      |
  |     | vars (e.g. portfolio  |      |
  |     | balance)              |      |
  |     +-----------+-----------+      |
  +-----------------|------------------+
                    |
          +---------+-----------+
          |                     |
          | record terminal     |
          | values              |
          v                     |
       ( final )                |
                                |
                                v
  +------------------------------------+
  | statistical summary of recorded    |
  | values                             |
  +------------------------------------+
```

The `simulate` macro in fincl assists in this. It takes in three required
things: a stochastic process to simulate paths for, a market that the simulation
will step forward from, and a list of dates. The date list is a list where
actions such as recording or trading should happen, rather than the user having
to compute the steps where the actions should happen manually. In addition to settings
dictating how the paths are generated, the macro does the rest of the logic as
well:

-   The `:state` clause gives a name to the process' states used in the simulation.
    This will depend on the stochastic process, but most processes expose the `:spot`
    price of the underlying asset (e.g. a stock price).
-   The `:portfolio` clause declares a portfolio that the simulation can trade
    within. You can declare multiple portfolios per simulation.
-   The `:hold` and `:accumulate` clauses declare local lisp objects and double-floats
    to use in the simulation.
-   The `:start` clause initializes values for `:held` and
    `:accumulate` symbols with access to the `market` on that day.
-   The `:every` clause specifies forms that will be evaluated at date in the date
    list. The `:on` clause is similar, but is run once for each day in the date
    list. The forms in both of these clauses have access to the `market` on this day.
-   The `:final` clause declares the terminal values to be returned from each path.
    Because of the element of randomness in stochastic differential equations,
    this will be a distribution of values, parameterized by path.

Inside the `:every` and `:on` forms, the user can evaluate `(record name value)`,
which stores `value` into `name` for summary. The complete collection of recorded
values is not available at the end of the simulation, only the mean, min/max,
standard deviation, and count of such values are returned. Unlike the `:final`
values, which return one value per path, the recorded values can be computed a
theoretical `n-paths` x `n-steps` times, which is expensive to store and usually not
enlightening anyway.


## Delta Hedging {#delta-hedging}

To see how these clauses interact, let's write a simulation that models the
strategy of _delta-hedging_ a short call position. Let's say we are a market maker
for equity options: we sell options to meet demand from market participants.
When we sell an option, we collect the value of the contract in cash, but expose
ourselves to directional movements of the underlying equity via option _delta_. We
are solely in the business of selling options, not betting on equity movements,
so we hedge away our delta by trading the underlying equity until our net delta
is \\(0\\).

Suppose we sell a call option that has a delta of \\(0.43\\). Our portfolio is now
 exposed to \\(-0.43\\) units of the underlying asset. We attain delta neutrality
 by buying \\(0.43\\) shares of the underlying at whatever price it's at.

Our portfolio now consists of \\(-1\\) call and \\(+0.43\\) shares; if the stock price drops
in the coming day and our option's delta drops to \\(0.37\\), our option only
exposes us to \\(-0.37\\) shares whilst we own \\(0.43\\) shares. The strategy
dictates to sell \\(0.06\\) shares of the underlying to maintain a net delta of
\\(0\\). We continue in this way, rebalancing our equity position to maintain
delta-neutrality.

Let's model this in fincl.

<details>
<summary>fincl setup</summary>
<div class="details">

```lisp
(ql:quickload :fincl)
(ql:quickload :fincl-viz)
(defpackage #:fincl-sims
  (:use #:cl #:fincl #:fincl-viz))
(in-package #:fincl-sims)
```
</div>
</details>

We declare our ambient market consisting of an underlying equity `$FOO` valued at
`$100`, with 20% volatility. `$FOO` does not pay a dividend. The risk-free interest
rate is `4%`. The call we will sell and hedge is a (European) `FOO 1Y 100C`.

```lisp
(defconstant +today+ (make-date 2026 11 01))
(defconstant +expiration+ (make-date 2027 11 01))
(defparameter *market* (make-market (valued-on +today+)
                                    (underlying :foo (spot 100)
                                                (vol 0.2d0))
                                    (curve :usd-ois (rate 0.04d0))))

(defconstant +option+ (make-option (call 100) (expires +expiration+)))
```

We will assume `$FOO` follows a geometric Brownian motion, and we rebalance delta
 as computed with the Black-Scholes formula (though fincl supports other delta
 computations). On each rebalance date, we will `record` the following:

-   Spot price of `$FOO`
-   Delta of our option
-   Value of our portfolio

Our `:final` value will be the net payoff of the strategy, both for the hedged
position and unhedged position (where we do not neutralize delta).

We have declared we will rebalance daily, so we will pass a list of dates to our
`:on` clauses.

```lisp
(defconstant +rebalance-dates+
  (loop for d = (date+ +today+ 1) then (date+ d 1)
      while (date< d +expiration+)
      collect d))
```

In summary, we will run the following experiment:

```text
Simulate 1000 instances of daily delta hedging a FOO 1Y 100C option, and return the distribution
of final portfolio values, recording portfolio value, option delta, and FOO spot price
on each hedging day.
```

```lisp
(let ((premium (price +option+ :black-scholes *market*)))
  (defparameter *result*
    (simulate ((make-gbm) *market* +rebalance-dates+
               :n-paths 1000 :keep (:paths 10) :seed 40)
      ;; Initialize portfolio
      (:portfolio book (make-portfolio "hedge" (make-position +option+ :quantity -1))
       :cash premium :engine :black-scholes :record (:value))

      ;; s = Modeled $FOO price
      (:state s :spot)

      ;; Daily rebalance and record
      (:on +rebalance-dates+ (i market)
           (let ((del (getf (greeks +option+ :black-scholes market :which '(:delta)) :delta)))
             ;; Reassess delta and rebalance the shares in the book
             (trade book (make-holding :underlying :foo)
                    :to del)
             (record :share-price s)
             (record :val (price book :black-scholes market))
             (record :delta del)))

      ;; Final payout return
      (:final (:hedged (price book :black-scholes market))
              (:unhedged (- premium (payoff-value (option-payoff +option+) s)))))))
```

We can now retrieve summary statistics about the `:final` values parameterized by each path. Mathematically, we should expect this strategy to return `$0`, due to risk-neutral pricing assumptions.

```lisp
(multiple-value-bind (mean se)
    (simulation-summary *result* (simulation-values *result* :hedged))
  (format nil "~&~(~A~): mean ~,4F  standard error ~,4F~%" :hedged mean se))
```

```text
hedged: mean -0.0141  standard error 0.0240
```

Neat! We have experimentally recovered evidence that delta hedging is a net-zero
strategy in a Black-Scholes world, since the average payoff over 1000 possible
futures is near \\(0\\).


## Visualization {#visualization}

You may have noticed something in our `simulate` call: the `:keep (:paths 10)`.
Remember earlier when I said that recorded values are not stored in the result
of a simulation? That wasn't strictly speaking true; you can store all recorded
values along \\(n\\) paths, where \\(n\\) is the number you pass to `:keep (:paths
...)`. You can then inspect and visualize these values at your leisure, which we
will do here.

`fincl-viz` is a sister package that is meant to interop with fincl and assist
with the visualization process. It writes data to json that can be fed into
plotly.js for rendering in the browser.

Here is a "dashboard" of sorts that uses our simulation result `*result*` from
earlier to plot a handful of recorded portfolio values, option deltas, and our
distribution of final payoffs. Try fiddling with the graphs to zoom, rescale, etc.

```lisp
;; some global theme settings...
(with-theme '(:roles (:paths (:width 1.5)
                      :mean (:color "#FF0000" :opacity 0.6
                             :dash "dot" :label "Mean")))
  (plot
   (grid (panel (paths *result* :n 5 :record :share-price :colors *path-colors*)
                :title "5 samples of underlying price"
                :ylabel "Price ($)")
         (panel (paths *result* :n 5 :record :val :colors *path-colors*)
                (mean-line *result* :val)
                :title "5 samples of portfolio value"
                :ylabel "Value ($)")
         (panel (paths *result* :n 5 :record :delta :colors *path-colors*)
                (mean-line *result* :delta)
                :title "5 samples of option deltas"
                :ylabel "Delta")
         (panel (histogram *result* :unhedged :color "#eb6834" :opacity 0.55)
                (histogram *result* :hedged :color "#2a78d6" :opacity 0.55)
                :title "Return of short call strategies (profit / premium)"
                :xlabel "Return")
         :columns 2)
   :title "Delta hedging ATM option under Black-Scholes"
   :layout '(:height 750)
   :name "delta-hedge-gbm" :display :json))
```

{{< plotly name="delta-hedge-gbm" height="750" >}}

We see catastrophic losses in the unhedged position on account of the fact that
an unhedged short position can incur infinite losses (this is sometimes called
selling a "naked" option).


### A Simple Path Generation Visualization {#a-simple-path-generation-visualization}

`simulate` can act as a simple path generation tool, forgoing any of the clauses
that actually update local variables. Here are `simulate` snippets that plot
paths and visualize them similar to my [last write-up](https://aatmunbaxi.netlify.app/comp/fast_monte_carlo_pricer/) on fincl's Monte Carlo
engine. All we need to do is record the spot at each date and keep as many paths
as we'd like to visualize:

```lisp
;; GBM path generation
(defparameter *gbm-result*
  (simulate ((make-gbm) *market* +dates+ :n-paths 30 :keep (:paths 30))
    (:state s :spot)
    (:every (date)
      (record :price s))))

;; Heston path generation
(defparameter *heston-result*
  (simulate ((make-heston :v0 0.04d0 :kappa 2d0
                          :theta 0.04d0 :xi 0.5d0 :rho -0.7d0)
             *market* +dates+ :n-paths 30 :keep (:paths 30))
    (:state s :spot)
    (:state v :variance)
    (:every (date)
      (record :price s)
      (record :vol (sqrt (max v 0d0))))))
```

And plotting them in a grid:

```lisp
(with-theme *sim-theme*
  (plot
   (grid (panel
          (paths *gbm-result* :n 20 :record :price :color *sim-colors*)
          (mean-line *gbm-result* :price)
          :title "Geometric Brownian Motion")
         (panel (paths *heston-result* :n 20 :record :price :color *sim-colors*)
                (mean-line *heston-result* :price)
                :title "Heston" :ylabel "S")
         :columns 2)
   :title "Stochastic Process Simulation"
   :layout '(:height 400)
   :name "gbm-heston-paths" :display :json))
```

{{< plotly name="gbm-heston-paths" height="400" >}}

I hope you agree that they look much nicer than they did before! Previously, I
was hacking together some matplotlib interop with lisp arrays. Now, it's writing
structured json with `yezon`.


## Concluding {#concluding}

So far fincl is shaping up to be a neat little tool for a tinkerer interested in
quant finance (did I mention `simulate` is extensible as well?). Simulations are
not limited to risk-neutral measure processes either. Recently, a change of
measure method was added to fincl so that a user can add risk premia to a
stochastic process. This opens up a number of avenues where simulations can be
made to better reflect the real world. I've had a lot of fun messing with it.
