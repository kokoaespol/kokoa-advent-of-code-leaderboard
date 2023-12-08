const ctx1 = document.getElementById('chart-p1');
const ctx2 = document.getElementById('chart-p2');

(async function() {
  const response = await fetch('/members');
  if (!response.ok) return;

  const members = await response.json();
  if (members.length === 0) {
    return;
  }

  const labels = [];
  const datasetsPartOne = [];
  const datasetsPartTwo = [];

  const datasetConfig = {
    pointStyle: 'circle',
    pointRadius: 5,
    pointHoverRadius: 10,
  }

  for (const member of members) {
    const name = member.name;
    const completion_day_level = member.completion_day_level;

    Object.keys(completion_day_level)
        .filter(day => !labels.includes(day))
        .forEach(day => labels.push(day));

    const partOne = new Array(25);
    const partTwo = new Array(25);

    for (const [day, stars] of Object.entries(completion_day_level)) {
      const dayNumber = parseInt(day);
      const dayDate = new Date(2023, 11, dayNumber).getTime();

      const partOneDate = parseInt(stars['1']?.get_star_ts) / 60;
      const partTwoDate = parseInt(stars['2']?.get_star_ts) / 60;

      partOne[dayNumber - 1] = partOneDate - (dayDate / 1000 / 60);
      partTwo[dayNumber - 1] = partTwoDate - (dayDate / 1000 / 60);
    }

    datasetsPartOne.push({label: name, data: partOne, ...datasetConfig});
    datasetsPartTwo.push({label: name, data: partTwo, ...datasetConfig});
  }

  const dataPartOne = {labels, datasets: datasetsPartOne};
  const dataPartTwo = {labels, datasets: datasetsPartTwo};

  const mkConfig = (data, title) => ({
    type: 'line',
    data: data,
    options: {
      plugins: {
        legend: {position: 'top'},
        title: {display: true, text: title},
        tooltip: true
      }
    }
  });

  new Chart(ctx1, mkConfig(dataPartOne, 'Part One'));
  new Chart(ctx2, mkConfig(dataPartTwo, 'Part Two'));
})();
