const TODAY = new Date("2023-04-14");
const TODAY_LOCALE = TODAY.toLocaleDateString("en-GB");

const EMACS_DATA = {
  labels: ["Editor/IDE", "File manager"],
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
    },
  ],
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
          min: new Date("2018-06"),
        },
        yAxis1: {
          ticks: {
            display: false,
          },
        },
        yAxis2: {
          ticks: {
            // display: false
          },
        },
      },
      plugins: {
        legend: {
          display: false,
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

document.addEventListener(
  "DOMContentLoaded",
  function () {
    emacsChart();
  },
  false
);
