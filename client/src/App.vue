<template>
  <div id="app">
    <div class="flex">
      <input type="text" class="" name="" id="">
      <button>Parse</button>
    </div>
    infix: {{ data.infix | infix }}
    <br />
    dnf: {{ data.dnf }}
    <Graph :graph="graphString" />
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
  components: {Graph},
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
      graphString: "",
      data: {
      }
    };
  },
  mounted() {
    axios
      .get("http://localhost:8080", {
        params: {
          input: ">(|(A,B),&(C,~(D)))"
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
      });
  }
};
</script>
