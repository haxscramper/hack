function getElem(elementName) { return document.getElementById(elementName); }

var field = [
  [ "", "", "" ],
  [ "", "", "" ],
  [ "", "", "" ],
];

var gameOver = false;

function fieldIsEmpty(row, col) { return field[row][col] === ""; }

function fieldSetCell(row, col) { field[row][col] = "x"; }

function fieldDoMove() {
  attempts = 1;
  var col = Math.floor(Math.random() * 3);
  var row = Math.floor(Math.random() * 3);
  while (!fieldIsEmpty(row, col) && attempts < 9) {
    ++attempts;
    col = Math.floor(Math.random() * 3);
    row = Math.floor(Math.random() * 3);
  }

  if (attempts > 9) {
    console.log("Made more than 9 attempts to find empty cell");
  }

  field[row][col] = "o";
}

function fieldIsFinished() {
  var dim = 3;
  // check horizontal
  for (row = 0; row < dim; ++row) {
    var rstring = field[row].join("");
    if (rstring === "xxx" || rstring === "ooo") {
      console.log("ok: " + rstring);
      return true;
    } else {
      console.log("Does not match:" + rstring);
    }
  }

  // check vertical
  console.log("Checking vertical");
  for (col = 0; col < dim; ++col) {
    var cstring = field[0][col] + field[1][col] + field[2][col];
    if (cstring === "xxx" || cstring === "ooo") {
      console.log("ok: " + cstring);
      return true;
    } else {
      console.log("Does not match:" + cstring);
    }
  }

  var ldiag = field[0][0] + field[1][1] + field[2][2];
  var rdiag = field[0][2] + field[1][1] + field[2][0];

  if (ldiag === "xxx"    //
      || ldiag === "ooo" //
      || rdiag === "xxx" //
      || rdiag === "ooo") {
    return true;
  } else {
    console.log("Does not match:" + ldiag);
    console.log("Does not match:" + rdiag);
  }

  return false;
}

function buttonPressed(rowIdx, colIdx) {
  if (gameOver) {
    return;
  }

  if (fieldIsEmpty(rowIdx, colIdx)) {
    fieldSetCell(rowIdx, colIdx);

    if (!fieldIsFinished()) {
      fieldDoMove();
    }

    gameOver = fieldIsFinished();

    if (gameOver) {
      console.log("Game over");
      alert("Game over");
    }

    generateTicTacTable();
  } else {
    console.log("cell already occupied do nothing");
  }
}

function generateTicTacTable() {
  var rowCount = field.length;
  var table = getElem("table");

  var tableHTML = "";
  for (var rowIdx = 0; rowIdx < field.length; ++rowIdx) {
    rowRes = [];
    for (var colIdx = 0; colIdx < field[rowIdx].length; ++colIdx) {
      var cell = field[rowIdx][colIdx];
      var onClickCall = "onclick=\"buttonPressed(" +
                        `${rowIdx}, ${colIdx}` +
                        ");\"";

      content = "<input type=\"button\" value=\"" + cell +
                "\" style=\"width: 50px; heght: 50px; font-size: 32px;\"" +
                onClickCall + ">";

      rowRes.push("<td>" + content + "</td>");
    }
    tableHTML += "<tr>" +
                 "<td>" + (rowCount - rowIdx) + "</td>" + rowRes.join("\n") +
                 "</tr>\n";
  }

  tableHTML += "<tr>" +
               "<td></td>" +
               field[0].map((r, idx) => "<td>" + (1 + idx) + "</td>").join("") +
               "</tr>";

  table.innerHTML = tableHTML;
}
