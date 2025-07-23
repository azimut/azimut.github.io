window.onload = function () {
  sortNotes();
  sortProjects();
  hookProgressBar();
};

// Reference: https://stackoverflow.com/questions/25175798/how-to-shuffle-a-nodelist
function sortNotes() {
  const notes = document.querySelector(".notes ul");
  if (!notes) return;
  for (let i = notes.children.length; i > 0; i--) {
    notes.appendChild(notes.children[(Math.random() * i) | 0]);
  }
}
function sortProjects() {
  const projects = document.querySelector(".projects-container");
  if (!projects) return;
  for (let i = projects.children.length; i > 0; i--) {
    projects.appendChild(projects.children[(Math.random() * i) | 0]);
  }
}

// https://eszter.space/progress-bar/
function hookProgressBar() {
  const content = document.getElementById("content");
  const progressBar = document.getElementById("progress-bar");
  if (!progressBar || !content) return;
  const distance =
    content.clientHeight + content.offsetTop - window.innerHeight;
  window.addEventListener("scroll", () => {
    const progress = (window.scrollY / distance) * 100;
    progressBar.style.width = `${progress}%`;
  });
}
