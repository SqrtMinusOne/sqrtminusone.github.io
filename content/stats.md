+++
title = "Stats"
norss = true
scripts = ["/js/chart.js", "/js/chartjs-adapter-date-fns.bundle.min.js", "/js/stats.js?v=6"]
+++

<style>
.charts-wrapper {
    margin-bottom: 16px;
}
</style>

These are some of my personal stats, collected from different sources and aggregated with my [DETERRED](https://github.com/sqrtMinusOne/deterred) package. They were last updated on **<span data-num="date">[REDACTED]</span>**.

<div hidden>
If I'm dead, may this serve as a reminder of who I was.
</div>

<noscript>
  <p>JavaScript is required to render these charts. Sorry.</p>
</noscript>

## Programming
### Programming languages

[WakaTime](https://wakatime.com/) is a service that tracks hours spent on programming, which I've used since late 2018. It allows exporting all your data for analysis, as all services should, but it's a proprietary service. If you want to reproduce this, you might also want to look at a compatible self-hosted implementation called [Wakapi](https://github.com/muety/wakapi).

My numbers are somewhat skewed towards `org-mode` because I switch to it all the time when I'm programming, and because I haven't configured WakaTime to account for AI usage.

Below are my top recorded languages:
<div class="charts-wrapper">
  <canvas id="chart-wakatime-top-languages" role="img" aria-label="The 15 language entries with the most recorded WakaTime hours."></canvas>
</div>

And the same data, but grouped by year:
<div class="charts-wrapper">
  <canvas id="chart-wakatime-top-languages-per-year" role="img" aria-label="WakaTime hours, stacked by recorded language or category and year."></canvas>
</div>

The chart below shows, for each month, the average age of the projects I worked on.

<div class="charts-wrapper">
  <canvas id="chart-wakatime-average-project-age-per-month" role="img" aria-label="Average age of projects with recorded WakaTime activity each month."></canvas>
</div>

### AI use

So far, my AI usage would have cost **<span data-num="ai-cost-total">$[REDACTED]</span>** at API prices (although I have, of course, used subscription plans). Below is the breakdown by month.

<div class="charts-wrapper">
  <canvas id="chart-ai-cost-by-model-per-month" role="img" aria-label="Monthly recorded AI cost, stacked by model."></canvas>
</div>

And the breakdown by token use:
<div class="charts-wrapper">
  <canvas id="chart-ai-tokens-by-model-per-month" role="img" aria-label="Monthly AI token usage, stacked by model."></canvas>
</div>

## Emacs use

[Emacs](https://www.gnu.org/savannah-checkouts/gnu/emacs/emacs.html) is the best thing to ever be created by an intelligent being in the whole Universe. Here's the percentage of my screen time I spent using Emacs:

<div class="charts-wrapper">
  <canvas id="chart-aw-emacs-fraction-per-month" role="img" aria-label="Emacs as a percentage of non-AFK computer time each month."></canvas>
</div>

This data was collected by [ActivityWatch](https://activitywatch.net/).

## Reading and listening

### Music

This chart shows the number of hours I've spent listening to music using [Google Play Music](https://en.wikipedia.org/wiki/Google_Play_Music) and later [MPD](https://www.musicpd.org/).

<div class="charts-wrapper">
  <canvas id="chart-mpd-listened-by-year" role="img" aria-label="Hours of music recorded by MPD each year."></canvas>
</div>

Here's that data split between albums that first appeared in my collection that year vs. those that had appeared in earlier years:

<div class="charts-wrapper">
  <canvas id="chart-mpd-new-albums-listened" role="img" aria-label="Music listening hours split by whether an album first appeared that year."></canvas>
</div>

### Podcasts

Here's the approximate number of hours I've spent listening to podcasts in [AntennaPod](https://antennapod.org/).

It's not quite accurate: the number for 2022 is understated because unsubscribing from some podcasts deleted their stats; the rest of the numbers might be inflated because they include time skipped by fast-forwarding as listening time. It also doesn't include podcasts I've listened to via YouTube or The Economist app. But it will do.

<div class="charts-wrapper">
  <canvas id="chart-podcasts-listened-by-year" role="img" aria-label="Recorded podcast listening time by year."></canvas>
</div>

Here's the same data broken down by language:

<div class="charts-wrapper">
  <canvas id="chart-podcasts-languages" role="img" aria-label="Podcast listening hours, stacked by language and year."></canvas>
</div>

And here are my top 15 podcasts, or most of them, anyway:

<div class="charts-wrapper">
  <canvas id="chart-podcasts-top-feeds" role="img" aria-label="The 15 podcast feeds with the most recorded listening time."></canvas>
</div>

### Articles

I read stuff on the Internet mostly via a read-it-later workflow, currently using a solution called [Readeck](https://readeck.org/en/).

I was inspired by [Tiago Forte's article](https://web.archive.org/web/20241105193056/https://fortelabs.com/blog/the-secret-power-of-read-it-later-apps/), which he has since deleted, though it remains available in the Internet Archive. So far I've read **<span data-num="read-it-later-count">[REDACTED]</span>** articles.

<div class="charts-wrapper">
  <canvas id="chart-read-it-later-articles-per-month" role="img" aria-label="Articles added to the read-it-later archive each month."></canvas>
</div>

And the same data broken down by language:

<div class="charts-wrapper">
  <canvas id="chart-read-it-later-articles-in-language-per-month" role="img" aria-label="Read-it-later articles, stacked by detected language each month."></canvas>
</div>

## Messengers

I would prefer email, but alas.

Here's the number of messages I sent and received per year:

<div class="charts-wrapper">
  <canvas id="chart-messengers-sent-received-per-year" role="img" aria-label="Recorded private messages by direction and year, with sent messages below the axis and received messages above it."></canvas>
</div>

And here's the combined number of sent and received messages, stacked by platform:

<div class="charts-wrapper">
  <canvas id="chart-messengers-messenger-per-year" role="img" aria-label="Sent and received messages combined, stacked by messaging platform and year."></canvas>
</div>

## Social media

Here's my social media activity grouped by month. Darker bands mean more activity.

<div class="charts-wrapper">
  <canvas id="chart-social-media-usage-bands" role="img" aria-label="Active social-media months by platform, with darker bands indicating greater activity within each activity type."></canvas>
</div>

## Movement
### Public transport

Here's my public transport use since January 2025:

<div class="charts-wrapper">
  <canvas id="chart-transport-trips-per-month-by-transport" role="img" aria-label="Recorded trips per month, stacked by transport type."></canvas>
</div>

### Bicycle

This is the distance covered by bicycle each month. Regrettably, the data only starts this year.

<div class="charts-wrapper">
  <canvas id="chart-fit-distance-per-month" role="img" aria-label="Monthly distance from recorded fitness activities."></canvas>
</div>

### University

Since January 2024 I've been to the uni **<span data-num="uni-visits">[REDACTED]</span>** times.

<div class="charts-wrapper">
  <canvas id="chart-uni-visits-per-month" role="img" aria-label="Recorded visits to the university each month."></canvas>
</div>

Here's this data broken down by transport type since January 2025:

<div class="charts-wrapper">
  <canvas id="chart-uni-visits-by-transport-per-month" role="img" aria-label="University visits per month, stacked by broad transport class."></canvas>
</div>

### Days away from home per year

I haven't been travelling all that much lately.

<div class="charts-wrapper">
  <canvas id="chart-locations-day-away-from-home-per-year" role="img" aria-label="Days spent away from home, grouped by year."></canvas>
</div>

## Work

This is me trying not to work on weekends, with varying success. Since I started recording, I've had **<span data-num="org-clock-free-weekends">[REDACTED]</span>** free weekends out of **<span data-num="org-clock-total-weekends">[REDACTED]</span>**, and **<span data-num="org-clock-free-weekends-q4">[REDACTED]</span>**/**<span data-num="org-clock-total-weekends-q4">[REDACTED]</span>** in the last 4 quarters.

<div class="charts-wrapper">
  <canvas id="chart-org-clock-free-weekends-per-quarter" role="img" aria-label="Free and non-free weekend days, with clocked weekend hours by quarter."></canvas>
</div>

This data comes from [org-clock](https://orgmode.org/manual/Clocking-commands.html), which is a time-tracking feature built into Emacs.

## Photos

Below is the number of photos I've taken per year. So far I've taken **<span data-num="digikam-photos">[REDACTED]</span>** photos. This comes from [digiKam](https://www.digikam.org/).

<div class="charts-wrapper">
  <canvas id="chart-digikam-photos-per-year" role="img" aria-label="Photos in the digiKam library, grouped by capture year."></canvas>
</div>
