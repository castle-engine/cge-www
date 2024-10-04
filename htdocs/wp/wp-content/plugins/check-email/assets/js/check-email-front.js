function check_email_rot47(str) {
    let rotated = '';
    for (let i = 0; i < str.length; i++) {
        const charCode = str.charCodeAt(i);
        if (charCode >= 33 && charCode <= 126) {
            rotated += String.fromCharCode(33 + ((charCode + 14) % 94));
        } else {
            rotated += str[i]; // Non-ROT47 characters remain unchanged
        }
    }
    return rotated;
}
document.addEventListener("DOMContentLoaded", function() {
    // functionality for rot13
    // this code for non-anchor email
    if (checkemail_encoder_data.is_enable && checkemail_encoder_data.email_technique == 'rot_13') {
        var emailLinks = document.querySelectorAll('.check-email-encoded-email');
        emailLinks.forEach(function(emailElement) {
            var encodedEmail = emailElement.textContent;
            var decodedEmail = encodedEmail.replace(/[a-zA-Z]/g, function(c) {
                return String.fromCharCode(
                    (c <= 'Z' ? 90 : 122) >= (c = c.charCodeAt(0) + 13) ? c : c - 26
                );
            });
            emailElement.textContent = decodedEmail;
        });
        //  this code for anchor tag
        document.querySelectorAll('a[href^="mailto:"]').forEach(function(emailElement) {
            var encodedEmail = emailElement.getAttribute('href').replace('mailto:', '');
            var decodedEmail = encodedEmail.replace(/[a-zA-Z]/g, function(c) {
                return String.fromCharCode(
                    (c <= 'Z' ? 90 : 122) >= (c = c.charCodeAt(0) + 13) ? c : c - 26
                );
            });
            // emailElement.textContent = decodedEmail;
            emailElement.setAttribute('href', 'mailto:' + decodedEmail);
        });
    }
    if (checkemail_encoder_data.is_enable && checkemail_encoder_data.email_technique == 'rot_47') {
        var emailLinks = document.querySelectorAll('.check-email-rot47-email');
        emailLinks.forEach(function(emailElement) {
            var encodedEmail = emailElement.textContent;
            const decodedEmail = check_email_rot47(encodedEmail);
            emailElement.textContent = decodedEmail;
        });

        document.querySelectorAll('a[href^="mailto:"]').forEach(function(emailElement) {
            var encodedEmail = emailElement.getAttribute('href').replace('mailto:', '');
            const decodedEmail = check_email_rot47(encodedEmail);
            emailElement.setAttribute('href', 'mailto:' + decodedEmail);
        });
    }
    if (checkemail_encoder_data.is_enable && checkemail_encoder_data.email_technique == 'css_direction') {
        //  this code for anchor tag
        document.querySelectorAll('a[href^="mailto:"]').forEach(function(encodedEmail) {
            encodedEmail.setAttribute("style", "direction: ltr;");
        });
    }
});