const TODAY = new Date("2023-04-14");
const TODAY_LOCALE = TODAY.toLocaleDateString("en-GB");

const EMACS_ITEM = {
  backgroundColor: "#8261bb",
  borderColor: "black",
  borderWidth: 0,
  borderSkipped: false,
  borderRadius: 0,
  datalabels: {
    color: "white",
  },
};

const COLORS = [
  "#77bceb",
  "#ff6384",
  "#77c0c0",
  "#ff9f40",
  "#ffcd56",
  "#c9cbcf",
];

let i = 0;

const EMACS_DATA = {
  labels: [
    "Editor/IDE",
    "File manager",
    "Email",
    "RSS",
    "Passwords",
    "Multimedia",
    "WM",
  ],
  datasets: [
    {
      label: "Jupyter",
      data: [
        {
          name: "Editor/IDE",
          span: [new Date("2018-10-24"), new Date("2021-04-01")],
        },
      ],
      yAxisID: "yAxis1",
    },
    {
      label: "NeoVim",
      data: [
        {
          name: "Editor/IDE",
          span: [new Date("2019-03-30"), new Date("2020-10-12")],
        },
      ],
      yAxisID: "yAxis1",
    },
    {
      label: "DataGrip",
      data: [
        {
          name: "Editor/IDE",
          span: [new Date("2020-02-01"), TODAY],
        },
      ],
      yAxisID: "yAxis1",
    },
    {
      label: "Emacs",
      data: [
        {
          name: "Editor/IDE",
          span: [new Date("2020-10-12"), TODAY],
        },
      ],
      yAxisID: "yAxis1",
      ...EMACS_ITEM,
    },
    {
      label: "vifm",
      data: [
        {
          name: "File manager",
          span: [new Date("2020-02-17"), new Date("2020-11-11")],
        },
      ],
      yAxisID: "yAxis2",
    },
    {
      label: "Dired",
      data: [
        {
          name: "File manager",
          span: [new Date("2020-11-11"), TODAY],
        },
      ],
      yAxisID: "yAxis2",
      ...EMACS_ITEM,
    },
    {
      label: "Mailspring",
      data: [
        {
          name: "Email",
          span: [new Date("2019-01-28"), new Date("2021-01-29")],
        },
      ],
      yAxisID: "yAxis3",
    },
    {
      label: "notmuch",
      data: [
        {
          name: "Email",
          span: [new Date("2021-01-29"), TODAY],
        },
      ],
      yAxisID: "yAxis3",
      ...EMACS_ITEM,
    },
    {
      label: "newsboat",
      data: [
        {
          name: "RSS",
          span: [new Date("2021-01-22"), new Date("2021-05-24")],
        },
      ],
      yAxisID: "yAxis4",
    },
    {
      label: "elfeed",
      data: [
        {
          name: "RSS",
          span: [new Date("2021-05-24"), TODAY],
        },
      ],
      yAxisID: "yAxis4",
      ...EMACS_ITEM,
    },
    {
      label: "Tiny Tiny RSS",
      data: [
        {
          name: "RSS",
          span: [new Date("2022-05-28"), TODAY],
          hint: "Sync with elfeed",
        },
      ],
      yAxisID: "yAxis4",
    },
    {
      label: "KeePassXC",
      data: [
        {
          name: "Passwords",
          span: [new Date("2019-01-31"), new Date("2021-07-26")],
        },
      ],
      yAxisID: "yAxis5",
    },
    {
      label: "password-store & pass.el",
      data: [
        {
          name: "Passwords",
          span: [new Date("2021-07-26"), TODAY],
        },
      ],
      yAxisID: "yAxis5",
    },
    {
      label: "MPD",
      data: [
        {
          name: "Multimedia",
          span: [new Date("2020-07-26"), TODAY],
        },
      ],
      yAxisID: "yAxis6",
    },
    {
      label: "ncmpcpp",
      data: [
        {
          name: "Multimedia",
          span: [new Date("2020-07-26"), new Date("2021-07-31")],
        },
      ],
      yAxisID: "yAxis6",
    },
    {
      label: "EMMS",
      data: [
        {
          name: "Multimedia",
          span: [new Date("2021-07-31"), TODAY],
        },
      ],
      yAxisID: "yAxis6",
      ...EMACS_ITEM,
    },
    {
      label: "MPV",
      data: [
        {
          name: "Multimedia",
          span: [new Date("2021-09-07"), TODAY],
        },
      ],
      yAxisID: "yAxis6",
    },
    {
      label: "Cinnamon",
      data: [
        {
          name: "WM",
          span: [new Date("2018-08-01"), new Date("2020-05-08")],
        },
      ],
      yAxisID: "yAxis7",
    },
    {
      label: "i3(-gaps)",
      data: [
        {
          name: "WM",
          span: [new Date("2020-05-08"), new Date("2021-11-14")],
        },
      ],
      yAxisID: "yAxis7",
    },
    {
      label: "EXWM",
      data: [
        {
          name: "WM",
          span: [new Date("2021-11-14"), TODAY],
        },
      ],
      yAxisID: "yAxis7",
      ...EMACS_ITEM,
    },
  ].map((d) => {
    if (!d.backgroundColor) {
      d.backgroundColor = COLORS[i];
    }
    i = (i + 1) % COLORS.length;
    return d;
  }),
};

function emacsChart() {
  const ctx = document.getElementById("chart-emacs-history");
  new Chart(ctx, {
    type: "bar",
    data: EMACS_DATA,
    plugins: [ChartDataLabels],
    options: {
      indexAxis: "y",
      grouped: true,
      aspectRatio: 1.2,
      parsing: {
        yAxisKey: "name",
        xAxisKey: "span",
      },
      layout: {
        padding: 1,
      },
      scales: {
        x: {
          type: "time",
          min: new Date("2018-09"),
        },
        ...Object.fromEntries(
          [1, 2, 3, 4, 5, 6].map((i) => [
            `yAxis${i}`,
            {
              display: false,
            },
          ])
        ),
      },
      plugins: {
        legend: {
          display: false,
        },
        title: {
          display: true,
          text: "Everything goes into Emacs",
          color: "black",
          font: {
            size: 15,
          },
        },
        datalabels: {
          formatter: function (value, context) {
            return context.dataset.label;
          },
          color: "black",
        },
        tooltip: {
          callbacks: {
            label: function (context) {
              const startDate = new Date(
                context.parsed._custom.start
              ).toLocaleDateString("en-GB");
              let endDate = new Date(
                context.parsed._custom.end
              ).toLocaleDateString("en-GB");
              if (endDate === TODAY_LOCALE) {
                endDate = "Today";
              }
              const label = context.dataset.label;
              return `${label}: ${startDate} - ${endDate}`;
            },
          },
        },
      },
    },
  });
}

async function emacsTimeChart() {
  const response = await fetch(
    "/data/2023-03-14-emacs/emacs-related-time-per-month.json"
  );
  const rawData = await response.json();
  const labels = [
    ["config_hours", "Config", "#A989C5"],
    ["package_hours", "Emacs Packages", "#7172AD"],
    ["orgmode_hours", "Org Mode", "#509EE3"],
    ["sqrt_hours", "sqrtminusone.xyz", "#51528D"],
    ["other_code_hours", "Other Code", "#F2A86F"],
    ["misc_hours", "Misc", "#F9D45C"],
  ];
  const data = {
    labels: rawData.map((d) => new Date(d["period"])),
    datasets: labels.map(([key, label, color]) => ({
      label,
      data: rawData.map((d) => ({
        period: new Date(d["period"]),
        value: d[key],
      })),
      backgroundColor: color,
    })),
  };

  const ctx = document.getElementById("chart-emacs-time");
  new Chart(ctx, {
    type: "bar",
    data,
    options: {
      parsing: {
        xAxisKey: "period",
        yAxisKey: "value",
      },
      scales: {
        x: {
          type: "time",
          min: data.labels[0],
          stacked: true,
        },
        y: {
          stacked: true,
          title: {
            display: true,
            text: "Hours",
          },
        },
      },
      plugins: {
        title: {
          display: true,
          text: "Emacs-related time per month",
          color: "black",
          font: {
            size: 15,
          },
        },
      },
    },
  });

  const rawStackedData = rawData.map((d) => {
    let sum = 0;
    for (const [key] of labels) {
      sum += d[key];
    }
    for (const [key] of labels) {
      d[key] /= sum;
    }
    return d;
  });
  const stackedData = {
    labels: rawData.map((d) => new Date(d["period"])),
    datasets: labels.map(([key, label, color]) => ({
      label,
      data: rawStackedData.map((d) => ({
        period: new Date(d["period"]),
        value: d[key],
      })),
      backgroundColor: color,
    })),
  };

  const stackedCtx = document.getElementById("chart-emacs-time-stacked");
  new Chart(stackedCtx, {
    type: "bar",
    data: stackedData,
    options: {
      parsing: {
        xAxisKey: "period",
        yAxisKey: "value",
      },
      scales: {
        x: {
          type: "time",
          min: data.labels[0],
          stacked: true,
        },
        y: {
          stacked: true,
          min: 0,
          max: 1,
          title: {
            display: true,
            text: "Hours (%)",
          },
        },
      },
      plugins: {
        title: {
          display: true,
          text: "Emacs-related time per month (stacked)",
          color: "black",
          font: {
            size: 15,
          },
        },
      },
    },
  });
}

async function configsChart() {
  const response = await fetch("/data/2023-03-14-emacs/lengths.csv");
  const csv = await response.text();
  const lines = csv.split("\n");
  const labels = lines[0].split(",");
  const rawData = lines
    .slice(1)
    .reverse()
    .map((line) => {
      const values = line.split(",");
      return Object.fromEntries(
        values.map((value, i) => {
          const key = labels[i];
          switch (key) {
            case "date":
              value = new Date(value);
              break;
            case "commit":
              break;
            default:
              value = Number(value);
              break;
          }
          return [key, value];
        })
      );
    });
  const data = {
    labels: rawData.map((d) => d.date),
    datasets: [
      {
        label: "Emacs.org",
        data: rawData.map((d) => ({
          x: d.date,
          y: d["Emacs.org"],
        })),
      },
      {
        label: "init.el",
        data: rawData.map((d) => ({
          x: d.date,
          y: d["init.el"],
        })),
      },
    ],
  };
  const ctx = document.getElementById("chart-emacs-config-size");
  new Chart(ctx, {
    type: "line",
    data,
    options: {
      pointRadius: 0,
      tension: 0.1,
      parsing: {
        xAxisKey: "x",
        yAxisKey: "y",
      },
      scales: {
        x: {
          type: "time",
          min: new Date("2020-10-01"),
        },
        y: {
          title: {
            display: true,
            text: "Lines of code",
          },
        },
      },
      plugins: {
        title: {
          display: true,
          text: "Emacs.org and init.el lengths",
          color: "black",
          font: {
            size: 15,
          },
        },
      },
    },
  });

  const emacsVimData = {
    datasets: [
      {
        label: "Emacs.org",
        data: rawData.map((d) => ({
          x: d.date,
          y: d["Emacs.org"],
        })),
        xAxisID: "xAxis1",
      },
      {
        label: "init.el",
        data: rawData.map((d) => ({
          x: d.date,
          y: d["init.el"],
        })),
        xAxisID: "xAxis1",
      },
      {
        label: "init.vim",
        data: rawData.map((d) => ({
          x: d.date,
          y: d["init.vim"],
        })),
        xAxisID: "xAxis2",
      },
    ],
  };
  const ctxEmacsVim = document.getElementById("chart-emacs-vim-config-size");
  new Chart(ctxEmacsVim, {
    type: "line",
    data: emacsVimData,
    options: {
      pointRadius: 0,
      tension: 0.1,
      parsing: {
        xAxisKey: "x",
        yAxisKey: "y",
      },
      scales: {
        xAxis1: {
          type: "time",
          min: new Date("2020-10-12"),
          max: new Date(
            new Date("2020-10-12").getTime() + 450 * (1000 * 60 * 60 * 24)
          ),
          display: false,
        },
        xAxis2: {
          type: "time",
          min: new Date("2019-03-30"),
          max: new Date(
            new Date("2019-03-30").getTime() + 450 * (1000 * 60 * 60 * 24)
          ),
          title: {
            display: true,
            text: "Days into",
          },
          ticks: {
            display: false,
          },
        },
        y: {
          title: {
            display: true,
            text: "Lines of code",
          },
        },
      },
      plugins: {
        title: {
          display: true,
          text: "Emacs vs. Vim config size",
          color: "black",
          font: {
            size: 15,
          },
        },
      },
    },
  });
}

document.addEventListener(
  "DOMContentLoaded",
  function () {
    emacsChart();
    emacsTimeChart();
    configsChart();
  },
  false
);
