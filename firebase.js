app.ports.firebaseWrite.subscribe(function(info) {
    if (info.tag == "write"){
        firebase.database().ref("/" + info.path).set(info.data);
    }
    else if (info.tag == "increment"){
        firebase.database().ref("/" + info.path).transaction(
            function(n){
                return (n + 1 || 1)
            })
    }

});

firebase.database().ref("/").on("value", function(snapshot) {
  app.ports.firebaseRead.send({tag : "", path : "", data : snapshot.val()});
});
