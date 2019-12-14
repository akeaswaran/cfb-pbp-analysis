CFB PBP Analysis
---

An assortment of random experiments done with college football play-by-play, drive-level, and game data from [CollegeFootballData.com](https://collegefootballdata.com).

All of this code is poorly commented and probably pretty poorly organized -- YMMV.

## Setup

After cloning the repo, you'll probably want to create a folder structure under the root directory that looks something like this:
```
- cfb-pbp-analysis (root dir)
  - data
    - games
    - drives
    - pbp
    - teams
```

Download the requisite files using the [exporter on CollegeFootballData.com](https://collegefootballdata.com/exporter) or via [the API](https://api.collegefootballdata.com). Do note that the API returns JSON and the exporter returns CSV, BUT the file import code in this repo usually expects CSV.

From either source, you're looking for game, drive, and play-by-play data from 2012 to 2019. Each download will take longer than the last, so keep that in mind.

Once you have all of those files downloaded, pull down the latest list of teams as well.

Put all of these files in the requisite folders in the structure, naming them `<year>.csv`. **Note: The teams one specifically will have to be named `2018.csv`.**

Once you do that, you are probably good to open any of the notebooks and run the cells.

## License

For more details, read `LICENSE.md`.
