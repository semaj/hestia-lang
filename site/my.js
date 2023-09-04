import init, { ev } from "./pkg/hestia_site.js";
var history = [];
var index = 0;
init().then(() => {
});
document.getElementById("prompt-input")
  .addEventListener("keyup", function(event) {
    event.preventDefault();
    if (event.keyCode === 13) {
      let inputValue = document.getElementById("prompt-input").value;
      let result = ev(inputValue);
      history.push(`hestia> ${inputValue}\n=> ${result}\n`); index++; document.getElementById("result").innerText = history.join("");
      var element = document.getElementById("repl");
      element.scrollTop = element.scrollHeight;
      document.getElementById("prompt-input").value = "";
    }
    // if (event.keyCode === 38) { // up
    //   if (index > 0) {
    //     document.getElementById("prompt-input").value = history.
    //   }
    // }
  });
