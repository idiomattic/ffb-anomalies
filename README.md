# Fantasy Football Statistical Anomaly Detector

A Clojure application that analyzes your Sleeper fantasy football league data to identify statistically anomalous stretches of games—whether you've been on an extraordinary scoring streak or faced unusually tough (or weak) opponents.

## What It Does

This tool fetches historical data from the Sleeper API for your fantasy football league and performs statistical analysis to detect:

- **High/low scoring streaks**: Consecutive weeks where you scored significantly above or below average
- **Opponent strength anomalies**: Consecutive weeks where you faced unusually strong or weak opponents

The app examines all possible consecutive game stretches (2-5 games) and calculates p-values to determine how statistically unlikely each stretch is based on the season's overall distribution.

## Dependencies

- **[hato](https://github.com/gnarroway/hato)**: HTTP client for API requests
- **[cheshire](https://github.com/dakrone/cheshire)**: JSON parsing
- **[aero](https://github.com/juxt/aero)**: Configuration management
- **[Apache Commons Math](https://commons.apache.org/proper/commons-math/)**: Statistical calculations
- **[core.memoize](https://github.com/clojure/core.memoize)**: Caching API responses

## How to Run

1. **Clone the repository**
   ```bash
   git clone <your-repo-url>
   cd fantasy-stats
   ```

2. **Configure your league ID**

   Export `SLEEPER_LEAGUE_ID` or edit `resources/config.edn` and replace the league ID with your Sleeper league ID:
   ```clojure
   {:league {:id #or [#env SLEEPER_LEAGUE_ID "1111111111111111111"]}
    :settings {:data-memoization-ttl-ms 60000
               :max-week 17}}
   ```

3. **Run the analysis**
   ```bash
   clj -M:run-m
   ```

   Or specify a custom p-value threshold (default is 0.01):
   ```bash
   clj -M:run-m 0.05
   ```

## Statistical Model

The application uses a **normal distribution model** to identify anomalies:

- Calculates mean and standard deviation for all points scored and points against across the season
- For multi-game stretches, adjusts the standard deviation using `σ_sum = σ × √n` (assuming independence)
- Computes z-scores and p-values for each consecutive stretch
- Reports stretches that fall below the specified p-value threshold (i.e., events that occur by chance less than the threshold percentage of the time)

Anomalies are classified by severity based on z-score:
- **EXTREME**: |z| ≥ 3.5 (≈ 1 in 5000 event)
- **VERY**: |z| ≥ 3.0 (≈ 1 in 741 event)
- **RARE**: |z| ≥ 2.5 (≈ 1 in 124 event)
- **UNUSUAL**: |z| ≥ 2.0 (≈ 1 in 44 event)

## Example Output

```
Using p-value threshold: 0.01 (finding events rarer than 1.0%)

Season: 2024
Points For - Mean: 115.32, Std Dev: 22.45
Points Against - Mean: 114.87, Std Dev: 21.89

Found 8 anomalous stretches:

[VERY] JohnDoe: Extremely HIGH scoring streak (weeks 8, 9, 10)
  Total: 412.50 (avg 137.50/week), p-value: 0.001234, z-score: 3.21
  ^ This is a 3.2-sigma event!
```

## License

[Your license here]
