(() => {
  "use strict";

  const DATA_ROOT = "/deterred-stats/";
  const DATA = new Map();
  const MOBILE = window.matchMedia("(max-width: 600px)").matches;
  const ASPECT = {
    normal: MOBILE ? 1 : 2,
    medium: MOBILE ? 0.85 : 1.8,
    tall: MOBILE ? 0.65 : 1.3,
  };
  const PALETTE = [
    "#509ee3",
    "#dc3545",
    "#73d9d9",
    "#ff9f40",
    "#8261bb",
    "#ffcd56",
    "#3a8f5c",
    "#e76f51",
    "#457b9d",
    "#a989c5",
    "#2a9d8f",
    "#f4a261",
    "#6d597a",
    "#ef476f",
    "#118ab2",
    "#8d99ae",
  ];
  const NAMED_COLORS = {
    Sent: "#dc3545",
    Received: "#509ee3",
    English: "#509ee3",
    Russian: "#dc3545",
    Unknown: "#8d99ae",
    "New albums": "#73d9d9",
    "Previously heard albums": "#8261bb",
    "Free weekend days": "#3a8f5c",
    "Weekend days with clocked work": "#ff9f40",
    Telegram: "#229ed9",
    Discord: "#5865f2",
    VK: "#0077ff",
    Metro: "#8261bb",
    Bus: "#509ee3",
    Tram: "#dc3545",
    Trolleybus: "#3a8f5c",
    Taxi: "#ff9f40",
    Bike: "#3a8f5c",
    "Surface transport": "#509ee3",
  };
  const LABELS = {
    en: "English",
    ru: "Russian",
    unknown: "Unknown",
    vk: "VK",
    telegram: "Telegram",
    discord: "Discord",
    bike: "Bike",
    metro: "Metro",
    "surface transport": "Surface transport",
  };
  const INTEGER = new Intl.NumberFormat("en-US", {
    maximumFractionDigits: 0,
  });
  const DECIMAL = new Intl.NumberFormat("en-US", {
    maximumFractionDigits: 2,
  });
  const COMPACT = new Intl.NumberFormat("en-US", {
    notation: "compact",
    maximumFractionDigits: 1,
  });
  const DATE = new Intl.DateTimeFormat("en-US", {
    dateStyle: "long",
    timeZone: "UTC",
  });

  function number(value) {
    const result = Number(value);
    if (!Number.isFinite(result)) {
      throw new Error(`Invalid numeric value: ${value}`);
    }
    return result;
  }

  function date(value) {
    const result = new Date(value);
    if (Number.isNaN(result.getTime())) {
      throw new Error(`Invalid date: ${value}`);
    }
    return result;
  }

  function monthDate(month) {
    return date(`${month}-01T00:00:00`);
  }

  function monthKey(value) {
    return `${value.getFullYear()}-${String(value.getMonth() + 1).padStart(
      2,
      "0"
    )}`;
  }

  function monthsBetween(first, last) {
    const months = [];
    const current = monthDate(first);
    const end = monthDate(last);

    while (current <= end) {
      months.push(monthKey(current));
      current.setMonth(current.getMonth() + 1);
    }
    return months;
  }

  function prettyLabel(value) {
    const text = String(value);
    return LABELS[text.toLowerCase()] || text.replace(/^anthropic\//, "");
  }

  function colorFor(value) {
    const label = prettyLabel(value);
    if (NAMED_COLORS[label]) {
      return NAMED_COLORS[label];
    }

    let hash = 0;
    for (const character of label) {
      hash = (hash * 31 + character.charCodeAt(0)) | 0;
    }
    return PALETTE[Math.abs(hash) % PALETTE.length];
  }

  function fade(color, opacity) {
    const alpha = Math.round(255 * opacity)
      .toString(16)
      .padStart(2, "0");
    return `${color}${alpha}`;
  }

  function formatValue(value, format) {
    if (value === null || value === undefined || Number.isNaN(Number(value))) {
      return "No data";
    }

    const numeric = Number(value);
    switch (format) {
      case "currency":
        return `$${numeric.toLocaleString("en-US", {
          minimumFractionDigits: 2,
          maximumFractionDigits: 2,
        })}`;
      case "tokens":
        return `${INTEGER.format(numeric)} tokens`;
      case "percent":
        return `${DECIMAL.format(numeric)}%`;
      case "hours":
        return `${DECIMAL.format(numeric)} h`;
      case "kilometers":
        return `${DECIMAL.format(numeric)} km`;
      case "months":
        return `${DECIMAL.format(numeric)} months`;
      case "decimal":
        return DECIMAL.format(numeric);
      default:
        return INTEGER.format(numeric);
    }
  }

  function formatTick(value, format, absolute = false) {
    const numeric = absolute ? Math.abs(Number(value)) : Number(value);
    return format === "currency"
      ? `$${COMPACT.format(numeric)}`
      : format === "percent"
        ? `${DECIMAL.format(numeric)}%`
        : COMPACT.format(numeric);
  }

  function tooltipLabel(format, axis = "y", absolute = false) {
    return (context) => {
      const prefix = context.dataset.label
        ? `${context.dataset.label}: `
        : "";
      const value = context.parsed[axis];
      return `${prefix}${formatValue(absolute ? Math.abs(value) : value, format)}`;
    };
  }

  function timeScale(periodCount) {
    return {
      type: "time",
      grid: { display: false },
      time: {
        unit: periodCount > 30 ? "year" : "month",
        displayFormats: { month: "MMM yyyy", year: "yyyy" },
      },
      ticks: {
        maxRotation: 0,
        maxTicksLimit: MOBILE ? 4 : 9,
      },
    };
  }

  function categoryScale() {
    return {
      grid: { display: false },
      ticks: {
        maxRotation: 0,
        maxTicksLimit: 12,
      },
    };
  }

  function chartOptions({
    format = "count",
    yTitle,
    stacked = false,
    legend = true,
    periods = [],
    monthly = false,
    hideZeroTooltip = false,
    absoluteValues = false,
    aspectRatio = ASPECT.normal,
  }) {
    return {
      aspectRatio,
      animation: { duration: 300 },
      interaction: {
        intersect: false,
        mode: "index",
      },
      plugins: {
        legend: {
          display: legend,
          position: "bottom",
          labels: {
            boxHeight: 10,
            boxWidth: 10,
            padding: 12,
          },
        },
        tooltip: {
          ...(hideZeroTooltip
            ? { filter: (context) => context.parsed.y !== 0 }
            : {}),
          callbacks: {
            label: tooltipLabel(format, "y", absoluteValues),
          },
        },
      },
      scales: {
        x: {
          ...(monthly ? timeScale(periods.length) : categoryScale()),
          stacked,
        },
        y: {
          beginAtZero: true,
          stacked,
          title: {
            display: true,
            text: yTitle,
          },
          ticks: {
            callback: (value) =>
              formatTick(value, format, absoluteValues),
          },
        },
      },
    };
  }

  function singleSeries(
    rawData,
    {
      periodKey,
      valueKey,
      type = "bar",
      label,
      yTitle,
      format = "count",
      color = PALETTE[0],
      completeMonths = false,
      missingMonthValue = null,
    }
  ) {
    const monthly = periodKey === "month";
    let periods = [...new Set(rawData.map((row) => row[periodKey]))].sort();
    const values = new Map(
      rawData.map((row) => [row[periodKey], number(row[valueKey])])
    );

    if (monthly && completeMonths && periods.length > 1) {
      periods = monthsBetween(periods[0], periods.at(-1));
    }

    const data = periods.map((period) => {
      const value = values.has(period) ? values.get(period) : missingMonthValue;
      return monthly ? { x: monthDate(period), y: value } : value;
    });

    return {
      type,
      data: {
        ...(monthly ? {} : { labels: periods }),
        datasets: [
          {
            label,
            data,
            borderColor: color,
            backgroundColor: fade(color, type === "line" ? 0.16 : 0.78),
            borderWidth: type === "line" ? 2 : 1,
            borderRadius: type === "bar" ? 2 : 0,
            fill: false,
            pointHoverRadius: 4,
            pointRadius: type === "line" ? (periods.length > 36 ? 0 : 2) : 0,
            spanGaps: false,
            tension: type === "line" ? 0.16 : 0,
          },
        ],
      },
      options: chartOptions({
        format,
        yTitle,
        legend: false,
        periods,
        monthly,
      }),
    };
  }

  function groupedSeries(
    rawData,
    {
      periodKey,
      seriesKey,
      valueKey,
      value,
      type = "bar",
      yTitle,
      format = "count",
      stacked = true,
      seriesLabel = prettyLabel,
      hideZeroTooltip = false,
      absoluteValues = false,
      aspectRatio = ASPECT.normal,
    }
  ) {
    const monthly = periodKey === "month";
    const periods = [...new Set(rawData.map((row) => row[periodKey]))].sort();
    const series = [...new Set(rawData.map((row) => row[seriesKey]))];
    const values = new Map();
    const totals = new Map();

    for (const row of rawData) {
      const amount = number(value ? value(row) : row[valueKey]);
      const key = `${row[periodKey]}\u0000${row[seriesKey]}`;
      values.set(key, (values.get(key) || 0) + amount);
      totals.set(row[seriesKey], (totals.get(row[seriesKey]) || 0) + amount);
    }

    const datasets = series
      .filter((item) => totals.get(item) !== 0)
      .map((item) => {
        const label = seriesLabel(item);
        const color = colorFor(label);
        return {
          label,
          data: periods.map((period) => {
            const amount = values.get(`${period}\u0000${item}`) || 0;
            return monthly ? { x: monthDate(period), y: amount } : amount;
          }),
          backgroundColor: fade(color, type === "line" ? 0.15 : 0.78),
          borderColor: color,
          borderWidth: type === "line" ? 2 : 1,
          borderRadius: type === "bar" ? 2 : 0,
          fill: false,
          pointRadius: type === "line" ? 2 : 0,
          tension: type === "line" ? 0.16 : 0,
        };
      });

    return {
      type,
      data: {
        ...(monthly ? {} : { labels: periods }),
        datasets,
      },
      options: chartOptions({
        format,
        yTitle,
        stacked,
        legend: datasets.length > 1,
        periods,
        monthly,
        hideZeroTooltip,
        absoluteValues,
        aspectRatio,
      }),
    };
  }

  function columnSeries(rawData, periodKey, columns, options) {
    const rows = rawData.flatMap((row) =>
      columns.map(({ key, label, transform }) => ({
        [periodKey]: row[periodKey],
        series: label,
        value: transform ? transform(row[key], row) : row[key],
      }))
    );
    return groupedSeries(rows, {
      ...options,
      periodKey,
      seriesKey: "series",
      valueKey: "value",
    });
  }

  function rankingChart(
    rawData,
    { labelKey, valueKey, yTitle, format, color }
  ) {
    const entries = rawData
      .map((row) => ({
        label: String(row[labelKey]),
        value: number(row[valueKey]),
      }))
      .sort((left, right) => right.value - left.value)
      .slice(0, 15);
    const totals = new Map();
    const indexes = new Map();

    for (const { label } of entries) {
      totals.set(label, (totals.get(label) || 0) + 1);
    }
    for (const entry of entries) {
      const index = (indexes.get(entry.label) || 0) + 1;
      indexes.set(entry.label, index);
      entry.displayLabel =
        totals.get(entry.label) > 1
          ? `${entry.label} (${index})`
          : entry.label;
    }

    return {
      type: "bar",
      data: {
        labels: entries.map(({ displayLabel }) => displayLabel),
        datasets: [
          {
            data: entries.map(({ value }) => value),
            backgroundColor: entries.map(({ label }) =>
              fade(color || colorFor(label), 0.78)
            ),
            borderRadius: 2,
          },
        ],
      },
      options: {
        aspectRatio: ASPECT.tall,
        indexAxis: "y",
        animation: { duration: 300 },
        plugins: {
          legend: { display: false },
          tooltip: {
            callbacks: {
              label: tooltipLabel(format, "x"),
            },
          },
        },
        scales: {
          x: {
            beginAtZero: true,
            title: {
              display: true,
              text: yTitle,
            },
            ticks: {
              callback: (value) => formatTick(value, format),
            },
          },
          y: {
            grid: { display: false },
            ticks: {
              autoSkip: false,
              callback(value) {
                const label = this.getLabelForValue(value);
                const limit = MOBILE ? 24 : 44;
                return label.length > limit
                  ? `${label.slice(0, limit - 3)}…`
                  : label;
              },
            },
          },
        },
      },
    };
  }

  function weekendChart(rawData) {
    return {
      type: "bar",
      data: {
        labels: rawData.map((row) => row.quarter),
        datasets: [
          {
            label: "Free weekend days",
            data: rawData.map((row) => number(row.free_weekends)),
            backgroundColor: fade(
              NAMED_COLORS["Free weekend days"],
              0.78
            ),
            borderColor: NAMED_COLORS["Free weekend days"],
            borderWidth: 1,
            borderRadius: 2,
          },
          {
            label: "Weekend days with clocked work",
            data: rawData.map((row) => number(row.non_free_weekends)),
            backgroundColor: fade(
              NAMED_COLORS["Weekend days with clocked work"],
              0.78
            ),
            borderColor: NAMED_COLORS["Weekend days with clocked work"],
            borderWidth: 1,
            borderRadius: 2,
          },
          {
            type: "line",
            label: "Clocked weekend hours",
            data: rawData.map((row) => number(row.weekend_hours)),
            yAxisID: "hours",
            borderColor: "#dc3545",
            backgroundColor: fade("#dc3545", 0.15),
            borderWidth: 2,
            pointRadius: 2,
            tension: 0.16,
          },
        ],
      },
      options: {
        aspectRatio: ASPECT.normal,
        interaction: {
          intersect: false,
          mode: "index",
        },
        plugins: {
          legend: {
            position: "bottom",
            labels: {
              boxHeight: 10,
              boxWidth: 10,
            },
          },
          tooltip: {
            callbacks: {
              label: (context) =>
                `${context.dataset.label}: ${formatValue(
                  context.parsed.y,
                  context.dataset.yAxisID === "hours" ? "hours" : "count"
                )}`,
            },
          },
        },
        scales: {
          x: {
            stacked: true,
            grid: { display: false },
          },
          y: {
            beginAtZero: true,
            stacked: true,
            title: {
              display: true,
              text: "Weekend days",
            },
            ticks: { precision: 0 },
          },
          hours: {
            beginAtZero: true,
            position: "right",
            grid: { drawOnChartArea: false },
            title: {
              display: true,
              text: "Clocked hours",
            },
          },
        },
      },
    };
  }

  function socialTimeline(rawData) {
    const activities = [
      ...new Map(
        rawData.map((row) => [
          row.activity_type,
          {
            type: row.activity_type,
            color: row.color,
            label: row.label,
            order: number(row.lane_order),
          },
        ])
      ).values(),
    ].sort((left, right) => left.order - right.order);
    const firstDate = date(
      `${rawData.map((row) => row.month_start).sort()[0]}T00:00:00`
    );
    const lastDate = date(
      `${rawData.map((row) => row.month_end).sort().at(-1)}T00:00:00`
    );

    return {
      type: "bar",
      data: {
        labels: activities.map(({ label }) => label),
        datasets: activities.map((activity) => ({
          label: activity.label,
          data: rawData
            .filter(
              (row) =>
                row.activity_type === activity.type && row.active !== 0
            )
            .map((row) => ({
              count: number(row.count),
              intensity: number(row.intensity),
              lane: row.label,
              month: row.month,
              span: [
                date(`${row.month_start}T00:00:00`),
                date(`${row.month_end}T00:00:00`),
              ],
            })),
          parsing: {
            xAxisKey: "span",
            yAxisKey: "lane",
          },
          backgroundColor: (context) =>
            fade(
              activity.color,
              0.22 + 0.78 * (context.raw?.intensity ?? 0.5)
            ),
          borderColor: activity.color,
          borderSkipped: false,
          borderWidth: 0,
          barPercentage: 1,
          categoryPercentage: 0.78,
          grouped: false,
        })),
      },
      options: {
        aspectRatio: ASPECT.medium,
        indexAxis: "y",
        animation: { duration: 300 },
        plugins: {
          legend: { display: false },
          tooltip: {
            callbacks: {
              title: (items) => items[0].raw.month,
              label: (context) =>
                `${context.raw.lane}: ${INTEGER.format(context.raw.count)}`,
            },
          },
        },
        scales: {
          x: {
            type: "time",
            min: firstDate,
            max: lastDate,
            grid: { display: false },
            time: {
              unit: "year",
              displayFormats: { year: "yyyy" },
            },
            ticks: {
              maxRotation: 0,
              maxTicksLimit: MOBILE ? 5 : 9,
            },
          },
          y: {
            grid: { display: false },
          },
        },
      },
    };
  }

  const CHARTS = {
    "ai-cost-by-model-per-month": (data) =>
      groupedSeries(data, {
        periodKey: "month",
        seriesKey: "model_name",
        valueKey: "cost",
        yTitle: "Cost (USD)",
        format: "currency",
        hideZeroTooltip: true,
        aspectRatio: ASPECT.medium,
      }),

    "ai-tokens-by-model-per-month": (data) =>
      groupedSeries(data, {
        periodKey: "month",
        seriesKey: "model_name",
        valueKey: "tokens",
        yTitle: "Tokens",
        format: "tokens",
        hideZeroTooltip: true,
        aspectRatio: ASPECT.medium,
      }),

    "aw-emacs-fraction-per-month": (data) =>
      singleSeries(data, {
        periodKey: "month",
        valueKey: "fraction",
        type: "line",
        label: "Emacs",
        yTitle: "Screen time",
        format: "percent",
        color: "#8261bb",
      }),

    "wakatime-average-project-age-per-month": (data) =>
      singleSeries(data, {
        periodKey: "month",
        valueKey: "age",
        type: "line",
        label: "Project age",
        yTitle: "Age (months)",
        format: "months",
        color: "#3a8f5c",
      }),

    "wakatime-top-languages-per-year": (data) =>
      groupedSeries(data, {
        periodKey: "year",
        seriesKey: "language",
        valueKey: "hours",
        yTitle: "Hours",
        format: "hours",
      }),

    "wakatime-top-languages": (data) =>
      rankingChart(data, {
        labelKey: "name",
        valueKey: "hours",
        yTitle: "Hours",
        format: "hours",
      }),

    "mpd-listened-by-year": (data) =>
      singleSeries(data, {
        periodKey: "year",
        valueKey: "total",
        label: "Music",
        yTitle: "Hours",
        format: "hours",
        color: "#8261bb",
      }),

    "mpd-new-albums-listened": (data) =>
      columnSeries(
        data,
        "year",
        [
          { key: "new", label: "New albums" },
          { key: "old", label: "Previously heard albums" },
        ],
        {
          yTitle: "Hours",
          format: "hours",
          stacked: true,
        }
      ),

    "podcasts-listened-by-year": (data) =>
      singleSeries(data, {
        periodKey: "year",
        valueKey: "hours",
        label: "Podcasts",
        yTitle: "Hours",
        format: "hours",
        color: "#509ee3",
      }),

    "podcasts-languages": (data) =>
      groupedSeries(data, {
        periodKey: "year",
        seriesKey: "language",
        valueKey: "hours",
        yTitle: "Hours",
        format: "hours",
      }),

    "podcasts-top-feeds": (data) =>
      rankingChart(data, {
        labelKey: "title",
        valueKey: "hours",
        yTitle: "Hours",
        format: "hours",
        color: "#509ee3",
      }),

    "read-it-later-articles-per-month": (data) =>
      singleSeries(data, {
        periodKey: "month",
        valueKey: "articles",
        type: "line",
        label: "Articles",
        yTitle: "Articles",
        color: "#3a8f5c",
      }),

    "read-it-later-articles-in-language-per-month": (data) =>
      groupedSeries(data, {
        periodKey: "month",
        seriesKey: "language",
        valueKey: "articles",
        yTitle: "Articles",
      }),

    "messengers-sent-received-per-year": (data) =>
      columnSeries(
        data,
        "year",
        [
          {
            key: "sent",
            label: "Sent",
            transform: (value) => -number(value),
          },
          { key: "received", label: "Received" },
        ],
        {
          yTitle: "Messages",
          format: "count",
          stacked: true,
          absoluteValues: true,
        }
      ),

    "messengers-messenger-per-year": (data) =>
      groupedSeries(data, {
        periodKey: "year",
        seriesKey: "messenger",
        value: (row) => number(row.sent) + number(row.received),
        yTitle: "Messages",
      }),

    "social-media-usage-bands": socialTimeline,

    "fit-distance-per-month": (data) =>
      singleSeries(data, {
        periodKey: "month",
        valueKey: "distance_km",
        label: "Distance",
        yTitle: "Distance (km)",
        format: "kilometers",
        color: "#3a8f5c",
      }),

    "transport-trips-per-month-by-transport": (data) =>
      groupedSeries(data, {
        periodKey: "month",
        seriesKey: "transport",
        valueKey: "trips",
        yTitle: "Trips",
      }),

    "uni-visits-per-month": (data) =>
      singleSeries(data, {
        periodKey: "month",
        valueKey: "visits",
        type: "line",
        label: "Visits",
        yTitle: "Visits",
        color: "#8261bb",
        completeMonths: true,
        missingMonthValue: 0,
      }),

    "uni-visits-by-transport-per-month": (data) =>
      groupedSeries(data, {
        periodKey: "month",
        seriesKey: "transport_class",
        valueKey: "visits",
        yTitle: "Visits",
      }),

    "org-clock-free-weekends-per-quarter": weekendChart,

    "digikam-photos-per-year": (data) =>
      singleSeries(data, {
        periodKey: "year",
        valueKey: "photos",
        label: "Photos",
        yTitle: "Photos",
        color: "#ff9f40",
    }),
  };

  async function loadData(name) {
    if (!DATA.has(name)) {
      DATA.set(
        name,
        fetch(`${DATA_ROOT}${name}.json`).then(async (response) => {
          if (!response.ok) {
            throw new Error(`HTTP ${response.status}`);
          }

          const data = await response.json();
          if (!Array.isArray(data) || data.length === 0) {
            throw new Error("The data file is empty");
          }
          return data;
        })
      );
    }
    return DATA.get(name);
  }

  function sum(data, key) {
    return data.reduce((total, row) => total + number(row[key]), 0);
  }

  async function fillNumbers() {
    const dateResponse = await fetch(`${DATA_ROOT}export-date.json`);
    if (!dateResponse.ok) {
      throw new Error(`HTTP ${dateResponse.status}`);
    }

    const [exportDate, costs, articles, visits, weekends, photos] =
      await Promise.all([
        dateResponse.text(),
        loadData("ai-cost-by-model-per-month"),
        loadData("read-it-later-articles-per-month"),
        loadData("uni-visits-per-month"),
        loadData("org-clock-free-weekends-per-quarter"),
        loadData("digikam-photos-per-year"),
      ]);
    const recentWeekends = [...weekends]
      .sort((left, right) => left.quarter.localeCompare(right.quarter))
      .slice(-4);
    const values = {
      date: DATE.format(new Date(`${exportDate.trim().slice(0, 10)}T00:00:00Z`)),
      "ai-cost-total": formatValue(sum(costs, "cost"), "currency"),
      "read-it-later-count": INTEGER.format(sum(articles, "articles")),
      "uni-visits": INTEGER.format(sum(visits, "visits")),
      "org-clock-free-weekends": INTEGER.format(
        sum(weekends, "free_weekends")
      ),
      "org-clock-total-weekends": INTEGER.format(
        sum(weekends, "weekend_days")
      ),
      "org-clock-free-weekends-q4": INTEGER.format(
        sum(recentWeekends, "free_weekends")
      ),
      "org-clock-total-weekends-q4": INTEGER.format(
        sum(recentWeekends, "weekend_days")
      ),
      "digikam-photos": INTEGER.format(sum(photos, "photos")),
    };

    for (const [name, value] of Object.entries(values)) {
      for (const element of document.querySelectorAll(
        `[data-num="${name}"]`
      )) {
        element.textContent = value;
      }
    }
  }

  function showError(name, message) {
    const canvas = document.getElementById(`chart-${name}`);
    if (!canvas) {
      return;
    }

    const error = document.createElement("p");
    error.setAttribute("role", "alert");
    error.textContent = message;
    canvas.replaceWith(error);
  }

  async function renderChart(name, build) {
    const canvas = document.getElementById(`chart-${name}`);
    if (!canvas) {
      console.error(`Missing canvas for ${name}`);
      return;
    }

    try {
      const data = await loadData(name);
      new Chart(canvas, build(data));
    } catch (error) {
      console.error(`Could not render ${name}.json:`, error);
      showError(name, "The chart could not be loaded.");
    }
  }

  async function initialize() {
    const numbers = fillNumbers().catch((error) => {
      console.error("Could not fill stats totals:", error);
    });

    if (typeof Chart === "undefined") {
      for (const name of Object.keys(CHARTS)) {
        showError(name, "Chart.js could not be loaded.");
      }
      await numbers;
      return;
    }

    Chart.defaults.color = "#222";
    Chart.defaults.font.family = getComputedStyle(document.body).fontFamily;
    await Promise.all([
      numbers,
      ...Object.entries(CHARTS).map(([name, build]) =>
        renderChart(name, build)
      ),
    ]);
  }

  document.addEventListener("DOMContentLoaded", initialize, false);
})();
