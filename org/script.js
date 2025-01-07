window.onload = function () {
  window.document.querySelectorAll("span.timestamp").forEach((span) => {
    const date = new Date(span.innerText.substr(1, 10));
    span.innerText = `${date.getFullYear() % 1000}/${date.getMonth()}`;
  });
};
