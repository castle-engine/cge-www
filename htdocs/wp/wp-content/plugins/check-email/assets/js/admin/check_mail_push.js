
document.addEventListener("DOMContentLoaded", function() {
    console.log('DOMContentLoaded')
    var ajax_url = checkemail_pushdata.ajax_url
    var ck_mail_security_nonce = checkemail_pushdata.ck_mail_security_nonce
    var firebaseConfig = checkemail_pushdata.fcm_config;

    // Initialize Firebase
    firebase.initializeApp(firebaseConfig);
    const messaging = firebase.messaging();

    


    function requestPermission() {
        const savedToken = localStorage.getItem("adminTokenCheckMail");
    
        if (savedToken) {
            console.log("Token already saved:");
        } else {
            if (Notification.permission === "granted") {
                console.log("Permission already granted.");
                messaging.getToken({ vapidKey: "AIzaSyDhRbFy9m-NXZVkozYJwKdDYJuwsL6W_bw" }).then(token => {
                    if (token) {
                        localStorage.setItem("adminTokenCheckMail", token);
                        saveAdminToken(token);
                    }
                }).catch(error => {
                    console.log("Error fetching token:", error);
                });
            } else if (Notification.permission === "denied") {
                console.log("Permission denied previously.");
            } else if (Notification.permission === "default") {
                console.log("Requesting permission...");
                Notification.requestPermission().then(permission => {
                    if (permission === "granted") {
                        messaging.getToken({ vapidKey: "AIzaSyDhRbFy9m-NXZVkozYJwKdDYJuwsL6W_bw" }).then(token => {
                            if (token) {
                                localStorage.setItem("adminTokenCheckMail", token);
                                saveAdminToken(token);
                            }
                        }).catch(error => {
                            console.log("Error fetching token:", error);
                        });
                    } else {
                        console.log("Notification permission denied.");
                    }
                }).catch(error => {
                    console.log("Error requesting permission:", error);
                });
            }
        }
        
    }
    


    function saveAdminToken(token) {
        fetch(ajax_url, {
            method: "POST",
            headers: { "Content-Type": "application/x-www-form-urlencoded" },
            body: "action=checkmail_save_admin_fcm_token&token=" + token+"&ck_mail_security_nonce="+ck_mail_security_nonce
        }).then(response => response.json()).then(data => {
            console.log("Token Saved:");
        });
    }
    // document.addEventListener("DOMContentLoaded", requestPermission);

    messaging.onMessage((payload) => {
        console.log("Message received. ", payload);
        new Notification(payload.notification.title, {
            body: payload.notification.body,
            icon: payload.notification.icon
        });
    });

    requestPermission();
})