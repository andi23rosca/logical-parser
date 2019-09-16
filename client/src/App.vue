<template>
  <div id="app">
    infix: {{ data.infix | infix }}
    <br />
    dnf: {{ data.dnf }}
  </div>
</template>

<script>
import axios from "axios";
import Viz from "viz.js";
import { Module, render } from "viz.js/full.render.js";

const viz = new Viz({ Module, render });

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
      data: {}
    };
  },
  mounted() {
    axios
      .get("http://localhost:8080", {
        params: {
          input: "&(A,B)"
        }
      })
      .then(r => {
        this.data = r.data;
        this.graphString = `
        graph logic {
          node [fontname = "Open Sans"]
          ${buildDotString(r.data.ast)}
        }
      `;
        viz
          .renderSVGElement(this.graphString)
          .then(function(element) {
            document.body.appendChild(element);
          })
          .catch(error => {
            // Create a new Viz instance (@see Caveats page for more info)
            viz = new Viz({ workerURL });

            // Possibly display the error
            console.error(error);
          });
      });
  }
};
</script>
