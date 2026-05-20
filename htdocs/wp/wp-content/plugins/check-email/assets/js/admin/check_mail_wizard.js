let currentStep = 1;
        var steps = ck_mail_wizard_data.steps;
        const ck_mail_security_nonce = ck_mail_wizard_data.ck_mail_security_nonce;

        function cm_showStep(step) {
            const stepContent = document.getElementById('step-content');
            const progressSteps = document.querySelectorAll('.cm_progress div');

            progressSteps.forEach((stepDiv, index) => {
                if (index < step) {
                    stepDiv.classList.add('active');
                } else {
                    stepDiv.classList.remove('active');
                }
            });

            stepContent.innerHTML = `
                <div class="cm_step">${steps[step - 1].title}</div>
                <h2 class="cm_H2">${steps[step - 1].heading}</h2>
                <div><form id="cm_step_form">
                <input type="hidden" name="action" value="check_mail_save_wizard_data" />
                <input type="hidden" name="ck_mail_security_nonce" value="${ck_mail_security_nonce}">
                ${steps[step - 1].content}</form></div>
            `;
            document.getElementById('cm_prevBtn').style.visibility = step === 1 ? 'hidden' : 'visible';
            document.getElementById('cm_nextBtn').innerText = step === steps.length ? 'Finish' : 'Save and Continue →';
        }

        function cm_nextStep() {
            if (currentStep < steps.length) {
                document.getElementById('cm-container-loader').style.display = 'block';
                currentStep++;
                cm_save_wizard();
                cm_showStep(currentStep);
            } else {
                cm_save_wizard();
            }
        }

        function cm_prevStep() {
            if (currentStep > 1) {
                currentStep--;
                cm_showStep(currentStep);
            }
        }

        document.addEventListener('DOMContentLoaded', function() {
            cm_showStep(currentStep);
        });

        function cm_save_wizard(){
            var t = jQuery("body" ).find("#cm_nextBtn");
            var data = jQuery("body" ).find("#cm_step_form").serialize();
            jQuery.ajax({
              url:ajaxurl,
              method:'post',
              dataType: "json",
              data:data,
              beforeSend: function(response){
                t.prop('disabled',true);
              },
              success:function(response){
                if (response.status == 200) {
                    steps = response.steps_data;
                    if (response.step == 'last') {
                        window.location = "admin.php?page=check-email-logs";
                    }
                }else{
                    console.log('something went wrong');
                }
              },
              complete:function(response){
                document.getElementById('cm-container-loader').style.display = 'none';
                t.prop('disabled',false);
              }               
            });
        }