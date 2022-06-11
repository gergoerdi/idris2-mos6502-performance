function timeIt() {
    let before = +new Date();
    let cnt = initialize(resolveFile);
    let after = +new Date();
    console.log("Idris2: " + cnt + " cycles done in " + (after - before) + "ms");
};
