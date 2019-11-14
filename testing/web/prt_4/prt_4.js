function update_image() {
    var selector   = document.getElementById("image_selector");
    var image_path = selector.options[selector.selectedIndex].value;
    document.getElementById("image").src = image_path;
}

function radio_select(value) {
    if (value === "1") {
        document.getElementById("image").src = "image_egg.tmp.png";
    } else {
        document.getElementById("image").src = "image_chicken.tmp.png";
    }
}

function evalTable() {
    var inVal = parseInt(document.getElementById("in_field").value);
    if (inVal < 0 || 255 < inVal) {
        alert("Веденное число должно находится в диапазоне [0, 255]");
    } else if (inVal === NaN) {
        alert("Could not parse number");
    } else {
        var base2 = (inVal).toString(2).split("").reverse().join("");
        var table = document.getElementById("table");
        for (var i = 0; i < 8; i++) {
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

function getElem(elementName) {
    return document.getElementById(elementName);
}

var boardSet = [
    [ 'w-p', 'w-p', 'w-p' ],
    [ '', '', '' ],
    [ 'b-h', 'b-h', "b-h" ],
];

var prevPos;

function dist(a, b) {
    return {r : Math.abs(a.r - b.r), c : Math.abs(a.c - b.c)};
}

/**
   Check if chess can move to posisiton `d` cells away
 */
function canGoThere(pieceCode, d) {
    console.log("Code:", pieceCode);
    console.log("Dist:", d);
    if (pieceCode.length != 3) {
        return false;
    } else {
        var last = pieceCode[2];
        switch (last) {
            case 'h':
                return (d.r == 2 && d.c == 1) || (d.r == 1 && d.c == 2);
            case 'b': return (d.r == d.c) && d.r > 0;
            case 'q':
                return (
                    (d.r == d.c && d.r > 0) || //
                    (d.r == 0 && d.c != 0) ||  //
                    (d.c == 0 && d.c != 0));
            case 'k': return d.r <= 1 && d.c <= 1;
            case 't':
                return (d.r == 0 && d.c != 0) || (d.c == 0 && d.c != 0);
            case 'p': return (d.r <= 1 && d.c == 0);
            default:
                console.log("Unknown piece type:", last);
                return false;
        }
        return true;
    }
}

function buttonPressed(rowIdx, colIdx) {
    nowPos = {r : rowIdx, c : colIdx};
    if (prevPos !== undefined) {
        // Previous button has already been pressed
        console.log("Dist:", dist(prevPos, nowPos));
        var code = boardSet[prevPos.r][prevPos.c];
        if (canGoThere(code, dist(prevPos, nowPos))) {
            console.log(
                "Can place chess piece", code, "to position", nowPos);

            tmp = boardSet[rowIdx][colIdx];
            boardSet[rowIdx][colIdx]       = code;
            boardSet[prevPos.r][prevPos.c] = tmp;

            generateChessTable();
        }
        prevPos = undefined; // Series of press has ended
    } else {
        // This is first button press in series;
        prevPos = nowPos;
    }
}

var chessMap = new Map([
    [ "b-k", "♚" ], // black-king
    [ "b-q", "♛" ], // black-queen
    [ "b-t", "♜" ], // black-tower
    [ "b-b", "♝" ], // black-bishop
    [ "b-h", "♞" ], // black-knight (aka 'horse' (for key // consistency))
    [ "b-p", "♟" ], // black-pawn
    [ "w-k", "♔" ], // white-king
    [ "w-q", '&#9812' ], // white-queen
    [ "w-t", "♖" ],      // white-tower
    [ "w-b", "♗" ],      // white-bishop
    [ "w-h", "♘" ], // white-knight (aka 'horse' (for key // consistency))
    [ "w-p", "♙" ]  // white-pawn
]);

function generateChessTable() {
    var rowCount = boardSet.length;
    var table    = getElem("table");

    var tableHTML = "";
    for (var rowIdx = 0; rowIdx < boardSet.length; ++rowIdx) {
        rowRes = [];
        for (var colIdx = 0; colIdx < boardSet[rowIdx].length; ++colIdx) {
            var cell    = boardSet[rowIdx][colIdx];
            var content = "default";
            if (Number.isInteger(cell)) {
                content = cell;
            } else {
                content = (chessMap.has(cell)) ? chessMap.get(cell) : cell;
                var onClickCall = "onclick=\"buttonPressed(" +
                                  `${rowIdx}, ${colIdx}` +
                                  ");\"";

                content = "<input type=\"button\" value=\"" +
                    content +
                    "\" style=\"width: 50px; heght: 50px; font-size: 32px;\"" +
                    onClickCall + ">";
            }

            rowRes.push("<td>" + content + "</td>");
        }
        tableHTML += "<tr>" +
                     "<td>" + (rowCount - rowIdx) + "</td>"
                     + rowRes.join("\n") + "</tr>\n";
    }

    tableHTML += "<tr>" +
                 "<td></td>"
                 + boardSet[0]
                       .map((r, idx) => "<td>" + (1 + idx) + "</td>")
                       .join("")
                 + "</tr>";

    table.innerHTML = tableHTML;
}
