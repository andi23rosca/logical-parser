<template>
  <div>
    <div></div>
  </div>
</template>

<script>
import Viz from "viz.js";
import { Module, render } from "viz.js/full.render.js";

export default {
  props: {
    graph: String
  },
  data() {
    return {
      viz: new Viz({ Module, render })
    };
  },
  watch: {
    graph: {
      handler(str) {
        this.viz
          .renderSVGElement(str)
          .then(el => {
            this.$el.removeChild(this.$el.firstElementChild);
            this.$el.appendChild(el);
          })
          .catch(error => {
            this.viz = new Viz({ Module, render });
            console.log(error);
          });
      }
    }
  }
};
</script>
