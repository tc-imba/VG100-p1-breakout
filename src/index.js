import 'typeface-roboto';
import 'normalize.css';

import './main.css';
import {Elm} from './Main.elm';
import * as serviceWorker from './serviceWorker';

import {Howl, Howler} from 'howler';

const sounds = {
  start: [
    new Howl({src: ['./sounds/start1.mp3']}),
    new Howl({src: ['./sounds/start2.mp3']}),
  ],
  hit: [
    new Howl({src: ['./sounds/hit1.mp3']}),
    new Howl({src: ['./sounds/hit2.mp3']}),
    new Howl({src: ['./sounds/hit3.mp3']}),
  ],
  miss: [
    new Howl({src: ['./sounds/miss1.mp3']}),
    new Howl({src: ['./sounds/miss2.mp3']}),
  ],
  combo: [
    new Howl({src: ['./sounds/combo1.mp3']}),
    new Howl({src: ['./sounds/combo2.mp3']}),
  ],
  pause: [
    new Howl({src: ['./sounds/pause.mp3']}),
  ],
  end: [
    new Howl({src: ['./sounds/end.mp3']}),
  ],
  reset: [
    new Howl({src: ['./sounds/reset.mp3']}),
  ],
};

const app = Elm.Main.init({
  node: document.getElementById('root'),
});

function getRandomInt(max) {
  return Math.floor(Math.random() * Math.floor(max));
}

app.ports.playSound.subscribe(function(name) {
  const soundArr = sounds[name];
  console.assert(soundArr != null, `Unknown sound: ${name}`);
  const sound = soundArr[getRandomInt(soundArr.length)];
  console.assert(sound != null, `Unknown sound: ${name}`);
  sound.play();
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
