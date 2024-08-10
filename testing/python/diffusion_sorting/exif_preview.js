function copyToClipboard(text) {
  navigator.clipboard.writeText(text).then(() => {
      alert(text + " copied to clipboard");
  }).catch(err => {
      console.error("Failed to copy: ", err);
  });
}

window.onload = function() {
  data = JSON.parse(document.getElementById("data").textContent);
}
