a = 2;
b = 3;
cond = false;

atmp = cond ? a : b;
btmp = cond ? b : a;

echo ("--");
echo (atmp);
echo (btmp);
echo ("--");

let(a = atmp, b = btmp) {
    echo (a);
    echo (b);
}
/*
a = atmp;
b = btmp;

echo (a);
echo (b);
*/