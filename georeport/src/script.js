// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2023 David Koňařík

function matchPriority(match) {
    const type = match.matchType.Case;
    if(type == "OsmByTag") return 1;
    if(type == "OsmByName") return 2;
    if(type == "ExternalById") return 3;
    if(type == "ExternalByName") return 4;
    throw `Unhandled match type: ${type}`;
}

function matchesPriority(matches) {
    // First is always best
    if(matches.length > 0) return matchPriority(matches[0]);
    return 9; // No match
}

function matchDescription(match, verbose) {
    if(match.matchType.Case == "OsmByTag") {
        return "OSM by tag";
    } else if(match.matchType.Case == "OsmByName") {
        return "OSM by name";
    } else if(match.matchType.Case == "ExternalById") {
        if(verbose) {
            return "External by ID: " + match.matchType.Fields[0];
        } else {
            return "External by ID";
        }
    } else if(match.matchType.Case == "ExternalByName") {
        if(verbose) {
            return "External by name: " + match.matchType.Fields[0];
        } else {
            return "External by name";
        }
    }
    throw "Unhandled matchType: " + match.matchType.Case;
}

function matchedName(match) {
	if(["OsmByName", "ExternalByName"].includes(match.matchType.Case)) {
		return match.matchType.Fields[1].stopName;
	}
	return "";
}

const MatchRow = {
    props: {
        matches: Array,
        match: Object,
    },
    methods: {
        matchDescription,
		matchedName,
    },
    setup({matches, match: selectedMatch}) {
        return {
            match: selectedMatch,
            showOnMap() {
                stopMarkers.clearLayers();
                function addMarker(match) {
                    L.marker([match.lat, match.lon], {
                        icon: match == selectedMatch
                              ? L.divIcon({
                                  className: "marker-selected-match",
                                  iconSize: null,
                              })
                              : L.divIcon({
                                  className: "marker-other-match",
                                  iconSize: null,
                              }),
                    }).bindTooltip(
						matchDescription(match, true) + "<br>"
						+ matchedName(match))
					.addTo(stopMarkers);
                }
                for(let match of matches) {
                    if(match == selectedMatch) {
                        // Make sure selected is on top
                        continue;
                    }
                    addMarker(match);
                }
                addMarker(selectedMatch);
                map.fitBounds(stopMarkers.getBounds());
            },
        };
    },
    template: `
        <tr>
            <td>{{ matchDescription(match, true) }}</td>
            <td>{{ matchedName(match) }}</td>
            <td v-if="['OsmByName', 'ExternalByName']
                      .includes(match.matchType.Case)">
                {{ match.matchType.Fields[1].score.toFixed(3) }}</td>
            <td v-else></td>
            <td><a v-if="['OsmByName', 'OsmByTag']
                         .includes(match.matchType.Case)"
                   :href="'https://www.openstreetmap.org/node/' + match.matchType.Fields[0]">
                OSM
            </a></td>
            <td><button @click="showOnMap()">Show</button></td>
        </tr>
    `,
};

const StopMatch = {
    props: {
        stop: Object,
        odd: Boolean,
    },
    components: {
        MatchRow,
    },
    methods: {
        bestMatchDescription(matches) {
            if(matches.length == 0) {
                return "No match";
            }
            // Matches are pre-sorted by the generator
            return matchDescription(matches[0]);
        },
        bestMatchClass(matches) {
            if(matches.length == 0) {
                return "match-None";
            }
            return "match-" + matches[0].matchType.Case;
        },
        radiusClass(radius) {
            if(radius < 0.01) {
                return "dist-0";
            } else if(radius < 0.05) {
                return "dist-1";
            } else if(radius < 0.1) {
                return "dist-2";
            } else {
                return "dist-3";
            }
        },
    },
    setup({stop, odd}) {
        const expanded = Vue.ref(false);
        return {
            stop, odd, expanded,
        };
    },
    template: `
        <tr class="stop" :class="{odd}">
            <td>{{stop.name}}</td>
            <td :class="bestMatchClass(stop.matches)">
                {{bestMatchDescription(stop.matches)}}
            </td>
            <td v-if="stop.radius !== null" :class="radiusClass(stop.radius)">
                {{stop.radius.toFixed(2)}}'
            </td>
            <td v-else></td>
            <td><button v-if="stop.matches.length > 0"
                        @click="expanded = !expanded">More</button></td>
        </tr>
        <tr class="stop-matches-detail" v-if="expanded">
            <td colspan="4">
                <table>
                    <tbody>
                        <match-row v-for="match in stop.matches"
                                   :matches="stop.matches" :match="match">
                        </match-row>
                    </tbody>
                </table>
            </td>
        </tr>
    `,
};

const StopsTable = {
    components: {
        StopMatch,
    },
    props: {
        stops: Array,
    },
    setup({ stops }) {
        const pageSize = 20;
        const page = Vue.ref(0);
        const query = Vue.ref("");
        const sortKey = Vue.ref(null);
        const sortMul = Vue.ref(1);
        const stopsFilteredSorted = Vue.computed(() => {
            let stopsfs = stops
            stopsfs =
                query.value
                ? stops.filter(s =>
                    s.name.toLowerCase().includes(query.value.toLowerCase()))
                : stops;

            if(sortKey.value == "name") {
                stopsfs.sort((s1, s2) =>
                    sortMul.value * s1.name.localeCompare(s2.name));
            } else if(sortKey.value == "match") {
                stopsfs.sort((s1, s2) =>
                    sortMul.value * (matchesPriority(s1.matches)
                                     - matchesPriority(s2.matches)));
            } else if(sortKey.value == "radius") {
                stopsfs.sort((s1, s2) =>
                    sortMul.value * (s1.radius - s2.radius));
            }
            return stopsfs;
        });
		const pageBounded = Vue.computed(() => {
            if((page.value + 1) * pageSize >= stopsFilteredSorted.value.length) {
                return 0;
            }
			return page.value;
		});
        const stopsPaged = Vue.computed(() => {
            return stopsFilteredSorted.value
                .slice(pageBounded.value * pageSize,
                       (pageBounded.value + 1) * pageSize);
        });
        return {
            page,
			pageBounded,
            query,
            stopsPaged,
            sortKey,
            sortBy(key) {
                if(sortKey.value == key) {
                    sortMul.value *= -1;
                } else {
                    sortMul.value = 1;
                }
                sortKey.value = key;
            },
            prevPage() {
                if(page.value > 0) {
                    page.value--;
                }
            },
            nextPage() {
                const nextPageOffset = (page.value + 1) * pageSize;
                if(nextPageOffset < stopsFilteredSorted.value.length) {
                    page.value++;
                }
            },
        };
    },
    template: `
        <div class="search">
            <input type="text"
                   placeholder="Search query"
                   v-model="query">
        </div>
        <table>
            <thead>
                <tr>
                    <th @click="sortBy('name')"
                        class="sortable"
                        :class="{sorting: sortKey == 'name'}">Name</th>
                    <th @click="sortBy('match')"
                        class="sortable"
                        :class="{sorting: sortKey == 'match'}">Best match</th>
                    <th @click="sortBy('radius')"
                        class="sortable"
                        :class="{sorting: sortKey == 'radius'}">Radius</th>
                    <th></th>
                </tr>
            </thead>
            <tbody>
                <stop-match v-for="(stop, i) in stopsPaged"
                            :key="stop.name"
                            :odd="i % 2 == 1"
                            :stop="stop">
                </stop-match>
            </tbody>
        </table>
        <div class="pagination">
            <button @click="prevPage()">←</button>
            <span class="pagenum">Page {{pageBounded + 1}}</span>
            <button @click="nextPage()">→</button>
        </div>
    `,
};

let map; // Global for Leaflet map
let stopMarkers;

document.addEventListener("DOMContentLoaded", () => {
    Vue.createApp({
        components: { StopsTable },
        setup() {
            Vue.onMounted(() => {
                map = L.map("map")
                    .setView([50, 15], 7);
                L.tileLayer("https://tile.openstreetmap.org/{z}/{x}/{y}.png", {
                    maxZoom: 19,
                    attribution: `© <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>`,
                }).addTo(map);
                stopMarkers = L.featureGroup().addTo(map);
            });
            return { railMatches, otherMatches };
        }
    }).mount("main");
});
