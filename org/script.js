window.onload = function () {
  sortNotes();
};

// Reference: https://stackoverflow.com/questions/25175798/how-to-shuffle-a-nodelist
function sortNotes() {
  const notes = document.querySelector(".notes ul");
  for (let i = notes.children.length; i > 0; i--) {
    notes.appendChild(notes.children[(Math.random() * i) | 0]);
  }
}
