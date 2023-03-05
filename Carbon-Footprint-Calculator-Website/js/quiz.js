function carbonFtScore(){
    
    // First question
    var radioButtons = document.querySelectorAll('input[name="zfav_language"]');
    var score = 0;

  for (var i = 0; i < radioButtons.length; i++) {
    if (radioButtons[i].checked) {
      score += parseInt(radioButtons[i].value);
      break;
    }
  }
  
  
  // Second question
  var radioButtons = document.querySelectorAll('input[name="afav_language"]');

  for (var i = 0; i < radioButtons.length; i++) {
    if (radioButtons[i].checked) {
      score += parseInt(radioButtons[i].value);
      break;
    }
  }

  // Third question
  var radioButtons = document.querySelectorAll('input[name="bfav_language"]');

  for (var i = 0; i < radioButtons.length; i++) {
    if (radioButtons[i].checked) {
      score += parseInt(radioButtons[i].value);
      break;
    }
  }

  // Fourth question
  var radioButtons = document.querySelectorAll('input[name="cfav_language"]');

  for (var i = 0; i < radioButtons.length; i++) {
    if (radioButtons[i].checked) {
      score += parseInt(radioButtons[i].value);
      break;
    }
  }

  // Fifth question
  var radioButtons = document.querySelectorAll('input[name="dfav_language"]');

  for (var i = 0; i < radioButtons.length; i++) {
    if (radioButtons[i].checked) {
      score += parseInt(radioButtons[i].value);
      break;
    }
  }

  // Sixth question
  var radioButtons = document.querySelectorAll('input[name="efav_language"]');

  for (var i = 0; i < radioButtons.length; i++) {
    if (radioButtons[i].checked) {
      score += parseInt(radioButtons[i].value);
      break;
    }
  }

  // Seventh question
  var radioButtons = document.querySelectorAll('input[name="ffav_language"]');

  for (var i = 0; i < radioButtons.length; i++) {
    if (radioButtons[i].checked) {
      score += parseInt(radioButtons[i].value);
      break;
    }
  }

  // Eighth question
  var radioButtons = document.querySelectorAll('input[name="gfav_language"]');

  for (var i = 0; i < radioButtons.length; i++) {
    if (radioButtons[i].checked) {
      score += parseInt(radioButtons[i].value);
      break;
    }
  }

  // Ninth question
  var radioButtons = document.querySelectorAll('input[name="hfav_language"]');

  for (var i = 0; i < radioButtons.length; i++) {
    if (radioButtons[i].checked) {
      score += parseInt(radioButtons[i].value);
      break;
    }
  }

  // Tenth question
  var radioButtons = document.querySelectorAll('input[name="ifav_language"]');

  for (var i = 0; i < radioButtons.length; i++) {
    if (radioButtons[i].checked) {
      score += parseInt(radioButtons[i].value);
      break;
    }
  }

  // Eleventh question
  var radioButtons = document.querySelectorAll('input[name="jfav_language"]');

  for (var i = 0; i < radioButtons.length; i++) {
    if (radioButtons[i].checked) {
      score += parseInt(radioButtons[i].value);
      break;
    }
  }

  // Twelfth question
  var radioButtons = document.querySelectorAll('input[name="kfav_language"]');

  for (var i = 0; i < radioButtons.length; i++) {
    if (radioButtons[i].checked) {
    score += parseInt(radioButtons[i].value);
      break;
    }
  }

  // Do something with the selected value
  document.getElementById("score_output").innerHTML=score;
}