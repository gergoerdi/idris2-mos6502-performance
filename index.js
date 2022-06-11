function CB_main($0) {
    let cb = n => w => { console.log(n); return 10 * n};
    let x1 = cbtest1(cb)    ;
    let x2 = cbtest2(cb)($0);

    console.log("CB_main", x1, x2);
};

function timeIt() {
    let before = +new Date();
    let cnt = initialize(resolveFile);
    let after = +new Date();
    console.log("Idris2: " + cnt + " cycles done in " + (after - before) + "ms");
};
