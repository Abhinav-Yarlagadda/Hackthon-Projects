// defines a function to generate random images
function display_random_image() 
{
     var ecoImages = [{
        src: "https://i.pinimg.com/564x/e0/ec/36/e0ec36b247a8c739ca24da1f16d6e65e.jpg",
        width: "500",
        height: "500",
        
    }, {
        src: "https://pbs.twimg.com/media/EzTlAy9VoAEXVlZ?format=png&name=small",
        width: "500",
        height: "500"
    }, {
        src: "https://pbs.twimg.com/media/EzTJzbwVUAANgel?format=png&name=small",
        width: "500",
        height: "500"
    }, {
        src: "https://pbs.twimg.com/media/EzTjVccVgA0NJpb?format=png&name=small",
        width: "500",
        height: "500"
    },{
        src: "https://images.squarespace-cdn.com/content/v1/5eda91260bbb7e7a4bf528d8/1618851402610-0PP8LHK779R3SEQZY797/meme18.jpg?format=500w",
        width: "500",
        height: "500"
    }, {
        src: "https://media.makeameme.org/created/recycling-is-too-5c0bc5.jpg",
        width: "500",
        height: "500"
    }, {
        src: "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSvUU1wsUWBQN8eYTdbULNuzVClp3cNn4VcDPgOg83L3-zb8jJdGh1gvzo4Ec1MgOOocoM&usqp=CAU",
        width: "500",
        height: "500"
    }, {
        src: "https://img-9gag-fun.9cache.com/photo/aV7ArXK_700bwp.webp",
        width: "500",
        height: "500"
    }, {
        src: "https://i.kym-cdn.com/photos/images/newsfeed/002/307/893/94f.png",
        width: "500",
        height: "500"
    }, {
        src: "https://i.redd.it/5vwf1vk6zd671.jpg",
        width: "500",
        height: "500"
    }, {
        src: "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTgYyWC9V7O1YEUBCQSuS2ALG5OfKub5B6jOg&usqp=CAU",
        width: "500",
        height: "500"
    }, {
        src: "https://consciouslyinspired.com/wp-content/uploads/2021/09/mother-nature-meme-10.jpeg",
        width: "500",
        height: "500"
    }, {
        src: "https://news.vcu.edu/image/4c943a47-8ecc-4618-bcf0-bd4a64d35c92",
        width: "500",
        height: "500"
    }, {
        src: "https://consciouslyinspired.com/wp-content/uploads/2021/09/mother-nature-meme-14.jpeg",
        width: "500",
        height: "500"
    }, {
        src: "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQPvI3lu2phiV-cdhva0kKalFU9K8gZNm9ZNQ&usqp=CAU",
        width: "500",
        height: "500"
    }, {
        src: "https://cdn.memes.com/up/84179561623508975/i/1624418618707.jpg",
        width: "500",
        height: "500"
    }, {
        src: "https://i.imgflip.com/2d14y2.jpg",
        width: "500",
        height: "500"
    }, {
        src: "https://i.redd.it/a7to1dj9s0d31.jpg",
        width: "500",
        height: "500"
    }];
    
    var preBuffer = [];
    for (var i = 0, j = ecoImages.length; i < j; i++) {
        preBuffer[i] = new Image();
        preBuffer[i].src = ecoImages[i].src;
        preBuffer[i].width = ecoImages[i].width;
        preBuffer[i].height = ecoImages[i].height;
    }
   
// create random image number
  function getRandomInt(min,max) 
    {
      //  return Math.floor(Math.random() * (max - min + 1)) + min;
    
imn = Math.floor(Math.random() * (max - min + 1)) + min;
    return preBuffer[imn];
    }  

// 0 is first image,   preBuffer.length - 1) is  last image
  
var newImage = getRandomInt(0, preBuffer.length - 1);
 
// remove the previous images
var images = document.getElementsByTagName('img');
var l = images.length;
for (var p = 0; p < l; p++) {
    images[0].parentNode.removeChild(images[0]);
}
// display the image  
document.body.appendChild(newImage);
}

// defines a function to generate random sounds
soundList = [ "sound1", "sound2", "sound3", "sound4", "sound5", "sound6", "sound7", "sound8", "sound9", "sound10", "sound11", "sound12" ]
function randomSound() {
    var audio1 = document.getElementById("sound1")
    audio1.pause()
    audio1.currentTime = 0.0
    var audio2 = document.getElementById("sound2")
    audio2.pause()
    audio2.currentTime = 0.0
    var audio3 = document.getElementById("sound3")
    audio3.pause()
    audio3.currentTime = 0.0
    var audio4 = document.getElementById("sound4")
    audio4.pause()
    audio4.currentTime = 0.0
    var audio5 = document.getElementById("sound5")
    audio5.pause()
    audio5.currentTime = 0.0
    var audio6 = document.getElementById("sound6")
    audio6.pause()
    audio6.currentTime = 0.0
    var audio7 = document.getElementById("sound7")
    audio7.pause()
    audio7.currentTime = 0.0
    var audio8 = document.getElementById("sound8")
    audio8.pause()
    audio8.currentTime = 0.0
    var audio9 = document.getElementById("sound9")
    audio9.pause()
    audio9.currentTime = 0.0
    var audio10 = document.getElementById("sound10")
    audio10.pause()
    audio10.currentTime = 0.0
    var audio11 = document.getElementById("sound11")
    audio11.pause()
    audio11.currentTime = 0.0
    var audio12 = document.getElementById("sound12")
    audio12.pause()
    audio12.currentTime = 0.0

    var index = Math.floor(Math.random() * 1000) % soundList.length;
    var id = soundList[index];
    var audio = document.getElementById(id);
    audio.play();
}
