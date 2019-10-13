function update_image () {
    var selector = document.getElementById("image_selector");
    var image_path = selector.options[selector.selectedIndex].value;
    document.getElementById("image").src = image_path;
}

function radio_select(value) {
    if(value === "1") {
        document.getElementById("image").src = "image_1.tmp.png";
    } else {
        document.getElementById("image").src = "image_2.tmp.png";
    }
}

function evalTable() {
    var inVal = parseInt(document.getElementById("in_field").value);
    if(inVal < 0 || 255 < inVal) {
        alert("sample text");
    } else if (inVal === NaN) {
        alert("Could not parse number");
    } else {
        var base2 = (inVal).toString(2).split("").reverse().join("");
        var table = document.getElementById("table");
        for(var i = 0; i < 8; i++) {
            var cell = table.rows[1].cells[8 - i];
            if (i >= base2.length || base2[i] === '0') {
                cell.innerHTML = '0';
                cell.setAttribute("class", "zerocell");
            } else {
                cell.innerHTML = '1';
                cell.setAttribute("class", "onecell");
            }
        }
    }
}
