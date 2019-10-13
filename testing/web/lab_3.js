function newCell(value) {
    var td = document.createElement("td");
    td.appendChild(document.createTextNode(value));
    return td;
}


function newHead(value) {
    var td = document.createElement("th");
    td.appendChild(document.createTextNode(value));
    return td;
}

function evalY(a, b, c, x) {
    return ((a * x + 3.8 * Math.tan(x)) / (Math.sqrt(b * x * x + c)));
}

function evalTable() {
    var table          = document.createElement('table');


    var start_x = parseFloat(
        document.getElementById('input_x_start').value);
    var final_x = parseFloat(
        document.getElementById('input_x_final').value);
    var x_step = parseFloat(document.getElementById('input_x_step').value);

    var a = parseFloat(document.getElementById('input_a').value);
    var b = parseFloat(document.getElementById('input_b').value);
    var c = parseFloat(document.getElementById('input_c').value);

    {
        var tr = document.createElement("tr");

        tr.appendChild(newHead("a"));
        tr.appendChild(newHead("b"));
        tr.appendChild(newHead("c"));
        tr.appendChild(newHead("x"));
        tr.appendChild(newHead("y"));

        table.appendChild(tr);
    }

    for (var current_x = start_x; current_x <= final_x;
         current_x += x_step) {
        var tr = document.createElement("tr");

        tr.appendChild(newCell(a));
        tr.appendChild(newCell(b));
        tr.appendChild(newCell(c));

        // var td_b = document.createElement("td");
        // var td_c = document.createElement("td");

        tr.appendChild(newCell(current_x));
        tr.appendChild(newCell(evalY(a, 1, 1, current_x).toFixed(3)));


        table.appendChild(tr);
    }

    document.body.appendChild(table);
}
