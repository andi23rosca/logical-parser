<template>
  <div
    class="table-grid"
    :style="{
      gridTemplateColumns: `repeat(${predicates.length + 1}, max-content)`
    }"
  >
    <div v-for="p in predicates" :key="p">
      {{ p }}
    </div>
    <div>{{ infix }}</div>
    <div style="display: contents" v-for="(row, i) in rows" :key="i + 'asd'">
      <div class="text-center" v-for="(a, x) in row" :key="i + x + 'col'">
        {{ a | simplify }}
      </div>
    </div>
  </div>
</template>

<script>
export default {
  filters: {
    simplify(n) {
      if (n == 2) return "*";
      return n;
    }
  },
  props: ["infix", "rows", "predicates"]
};
</script>
<style scoped>
.table-grid {
  display: grid;
  column-gap: 1rem;
  row-gap: 0.1rem;
}
</style>
