window.onload = function () {
  fixTimestamps();
  sortNotes();
};

// Reference: https://stackoverflow.com/questions/25175798/how-to-shuffle-a-nodelist
function sortNotes() {
  const notes = document.querySelector("#text-notes ul");
  for (let i = notes.children.length; i > 0; i--) {
    notes.appendChild(notes.children[(Math.random() * i) | 0]);
  }
}

function fixTimestamps() {
  document.querySelectorAll("span.timestamp").forEach((span) => {
    const date = new Date(span.innerText.substr(1, 10));
    span.innerText = `${date.getFullYear() % 1000}/${date.getMonth()}`;
  });
}
