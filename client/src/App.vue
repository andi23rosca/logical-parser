<template>
  <div id="app" class="mt-5 border container mx-auto p-5 shadow-lg rounded">
    <h1 class="text-xl font-bold text-purple-600 mb-4">Logical parser</h1>
    <div class="flex items-center mb-5">
      <input
        v-model="input"
        type="text"
        class="outline-none border focus:shadow-outline rounded bg-gray-200 px-2 py-1 w-64"
        placeholder="Enter formula here"
        autofocus
      />
      <button
        class="bg-purple-600 border border-purple-700 rounded px-3 ml-4 py-2 text-white outline-none focus:shadow-outline"
        @click="parse"
      >
        Parse
      </button>
      <div v-if="!isValid" class="text-red-600 ml-10">Formula is not valid</div>
    </div>
    <div v-if="!fetching" class="mt-10">
      <h2 class="text-lg font-bold text-purple-600 mb-3">Results</h2>
      infix: {{ data.infix | infix }}
      <br />
      predicates: {{ data.predicates.join(",") }}
      <br />
      hex: {{ data.hex }}
      <br />
      dnf: {{ data.dnf }}
      <br />
      simplified dnf: {{ data.simplifiedDnf }}
    </div>
    <div v-show="!fetching">
      <Graph :graph="graphString" />
    </div>
  </div>
</template>

<script>
import axios from "axios";
import Graph from "./Graph.vue";

const binaryList = ["or", "and", "implication", "eql", "nand"];
const nameToSymbol = name => {
  switch (name) {
    case "or":
      return "|";
    case "and":
      return "&";
    case "implication":
      return ">";
    case "eql":
      return "=";
    case "nand":
      return "%";
    default:
      break;
  }
};

const getUniqueName = () => Math.round(Math.random() * 10000000);

function buildDotString(ast, name = getUniqueName()) {
  if (Array.isArray(ast)) {
    if (binaryList.includes(ast[0])) {
      const child1 = getUniqueName();
      const child2 = getUniqueName();
      return `
        ${name} [label = "${nameToSymbol(ast[0])}"]
        ${name} -- ${child1}
        ${name} -- ${child2}

        ${buildDotString(ast[1], child1)}
        ${buildDotString(ast[2], child2)}
      `;
    }
    if (ast[0] === "not") {
      const child1 = getUniqueName();
      return `
        ${name} [label = "~"]
        ${name} -- ${child1}

        ${buildDotString(ast[1], child1)}
      `;
    }
  }
  return `${name} [label = "${ast.replace("*", "")}"]`;
}

export default {
  components: { Graph },
  filters: {
    infix(str) {
      if (str === undefined) return "";
      str = str.replace(/[&]/, "⋀");
      str = str.replace(/[~]/, "¬");
      str = str.replace(/[|]/, "⋁");
      str = str.replace(/[>]/, "⇒");
      str = str.replace(/[=]/, "⇔");
      return str;
    }
  },
  data() {
    return {
      input: ">(|(A,B),&(C,~(D)))",
      fetching: true,
      isValid: true,
      graphString: "",
      data: {
        isValid: true,
        infix: "",
        ast: [],
        predicates: [],
        truthTable: [],
        simplifiedTruthTable: [],
        hex: "",
        dnf: "",
        nandified: ""
      }
    };
  },
  computed: {
    hasPredicates() {
      return this.data.predicates.length > 0;
    }
  },
  methods: {
    parse() {
      this.fetching = true;
      axios
        .get("http://localhost:8080", {
          params: {
            input: this.input
          }
        })
        .then(r => {
          this.data = r.data;
          this.graphString = `
          graph logic {
            node [fontname = "Arial"]
            ${buildDotString(r.data.ast)}
          }
        `;
          this.fetching = false;
        })
        .catch(() => {
          this.isValid = false;
        });
    }
  }
};
</script>
