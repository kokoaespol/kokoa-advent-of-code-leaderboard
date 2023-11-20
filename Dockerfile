FROM haskell:9.2.8-slim

WORKDIR /app

# Update cabal index
RUN cabal update

# Cache dependencies
COPY aoc-leaderboard.cabal aoc-leaderboard.cabal
RUN cabal build --only-dependencies -j4

# Build the exe
COPY . .
RUN cabal install -j4

ENTRYPOINT ["aoc-leaderboard"]
