"use strict";

function setupRouteDetails(routeDetails) {
    routeDetails.addEventListener("toggle", () => {
        const isLoaded = !!routeDetails.querySelector(".trips-list");
        if(isLoaded || !routeDetails.open) return;
        const pageQs = location.search.substring(1);
        const tripId = routeDetails.dataset.id;
        fetch(`/Routes/TripsList/?${pageQs}&tripId=${tripId}`)
        .then(resp => resp.text())
        .then(text => {
            const doc = new DOMParser().parseFromString(text, "text/html");
            routeDetails.appendChild(doc.body.children[0]);
        });
    });
}

function showTable(show) {
    for(const table of document.querySelectorAll("table")) {
        table.style.display = show ? null : "none";
    }
}

function setupShowTable(checkbox) {
    checkbox.addEventListener("change", () => {
        showTable(checkbox.checked);
    });
}

function delayLineChartOpts(data, extraDatasets) {
    const ys = data.data.map(d => d.y);
    const maxY = Math.max(...ys);
    const minY = Math.min(...ys);
    const labelMap = Object.fromEntries(
        data.labels.map(l => [l.x, l.label]));
    return {
        type: "line",
        data: {
            datasets: [
                {
                    data: data.data,
                    pointRadius: 0,
                    segment: {
                        borderColor: ctx => {
                            const y = Math.min(ctx.p0.raw.y, ctx.p1.raw.y);
                            if(y < 0) return "#1ae0d3";
                            if(y < 5) return "#1ae070"
                            if(y < 10) return "#e0d31a";
                            return "#e01a28";
                        }
                    },
                },
                ...extraDatasets,
            ],
        },
        options: {
            scales: {
                x: {
                    type: "linear",
                    suggestedMin: 0,
                    suggestedMax: data.labels[data.labels.length - 1].x,
                    afterBuildTicks: axis =>
                        axis.ticks = data.labels.map(l => ({value: l.x})),
                    ticks: {
                        callback: x => labelMap[x],
                        minRotation: 80,
                        maxRotation: 80,
                    },
                    grid: {
                        color: "#ddd",
                    },
                },
                y: {
                    type: "linear",
                    suggestedMin: minY < -5 ? minY - 1 : -5,
                    suggestedMax: maxY > 10 ? maxY + 1 : 10,
                    grid: {
                        color: ctx =>
                            ctx.tick.value === 0 ? "#888" : "#ddd",
                        lineWidth: ctx =>
                            ctx.tick.value === 0 ? 2 : 1,
                    },
                    ticks: {
                        stepSize: 5,
                    },
                },
            },
            plugins: {
                legend: {
                    display: false,
                },
                tooltip: {
                    displayColors: false,
                    callbacks: {
                        title: ctx => {
                            if(ctx.length > 0) {
                                return ctx[0].raw.stopName;
                            }
                        },
                        label: ctx => {
                            if(ctx.datasetIndex === 0)
                                return `Median delay: ${ctx.raw.y} min`;
                            if(ctx.datasetIndex === 1)
                                return "15th% delay: "
                                    + `${ctx.raw.p15Delay.toFixed(2)} min`;
                            if(ctx.datasetIndex === 2)
                                return "85th% delay: "
                                    + `${ctx.raw.p85Delay.toFixed(2)} min`;
                        },
                    },
                },
            },
            interaction: {
                intersect: false,
                mode: "index",
            },
            animation: false,
        },
    };
}

function showChart(type) {
    if(type === "lineAndBounds") {
        for(const c of document.querySelectorAll(".delay-chart")) {
            c.style.display = null;
            setupCharts();
        }
        for(const c of document.querySelectorAll(".delay-heatmap")) {
            c.style.display = "none";
        }
    }
    if(type === "heatmap") {
        for(const c of document.querySelectorAll(".delay-chart")) {
            c.style.display = "none";
        }
        for(const c of document.querySelectorAll(".delay-heatmap")) {
            c.style.display = null;
            c.src = c.dataset.src;
        }
    }
}

function setupChartType(select) {
    select.addEventListener("change", () => {
        showChart(select.value);
    });
}

function setupSingleTripChart(chartElem) {
    if(chartElem.style.display === "none" || chartElem.dataset.setup) return;
    chartElem.dataset.setup = true;

    fetch(location + "/chartData")
    .then(resp => resp.json())
    .then(data => {
        new Chart(chartElem, delayLineChartOpts(data, []));
    })
    .catch(err => {
        chartElem.innerText = `Failed to get data for chart: ${err}`;
    });
}

function setupAggTripChart(chartElem) {
    if(chartElem.style.display === "none" || chartElem.dataset.setup) return;
    chartElem.dataset.setup = true;

    const ssgDate = chartElem.dataset.ssgDate;
    const pageQs = location.search.substring(1);
    fetch(`${location.pathname}/chartData?${pageQs}&ssgDate=${ssgDate}`)
    .then(resp => resp.json())
    .then(data => {
        new Chart(chartElem, delayLineChartOpts(data, [
            {
                data: data.data,
                parsing: {
                    yAxisKey: "p15Delay",
                },
                borderWidth: 0,
                pointRadius: 0,
                pointHoverRadius: 0,
                order: -1,
            },
            {
                data: data.data,
                parsing: {
                    yAxisKey: "p85Delay",
                },
                fill: {
                    target: "-1",
                    above: "#dddddd88",
                },
                borderWidth: 0,
                pointRadius: 0,
                pointHoverRadius: 0,
            },
        ]));
    })
    .catch(err => {
        chartElem.innerText = `Failed to get data for chart: ${err}`;
    });
}

function setupCharts() {
    if(location.pathname.startsWith("/Trip/")) {
        setupSingleTripChart(document.querySelector(".delay-chart"));
    }
    if(location.pathname.startsWith("/Trips/")) {
        for(const chartElem of document.querySelectorAll(".delay-chart")) {
            setupAggTripChart(chartElem);
        }
    }
}

document.addEventListener("DOMContentLoaded", () => {
    for(const rd of document.querySelectorAll("details.route-heading")) {
        setupRouteDetails(rd);
    }
    for(const c of document.querySelectorAll(".show-table input")) {
        showTable(c.checked);
        setupShowTable(c);
    }
    for(const s of document.querySelectorAll(".chart-type select")) {
        showChart(s.value);
        setupChartType(s);
    }
});

// Ran after all assets loaded, including Chart.js library
window.addEventListener("load", () => {
    setupCharts();
});