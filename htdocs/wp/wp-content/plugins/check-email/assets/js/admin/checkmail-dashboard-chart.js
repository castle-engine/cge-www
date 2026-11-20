document.addEventListener('DOMContentLoaded', function () {
    var ajaxurl = checkmail_chart.ajax_url;
    var ck_mail_security_nonce = checkmail_chart.ck_mail_security_nonce;
    let chartInstance;

    function createOrUpdateChart(data) {
        const ctx = document.getElementById('checkmail-dashboard-chart').getContext('2d');
        if (chartInstance) {
            chartInstance.destroy();
        }
        chartInstance = new Chart(ctx, {
            type: 'line',
            data: {
                labels: data.labels,
                datasets: [
                    {
                        label: 'Emails Sent',
                        data: data.sent,
                        borderColor: 'green',
                        fill: false,
                    },
                    {
                        label: 'Failed Emails',
                        data: data.failed,
                        borderColor: 'red',
                        fill: false,
                    },
                ],
            },
            options: {
                responsive: true,
                plugins: {
                    legend: {
                        display: true,
                    },
                },
            },
        });
        jQuery('#js_checkmail_total').html(data.total_mail +' Total');
        jQuery('#js_checkmail_sent').html(data.total_sent +' Sent');
        jQuery('#js_checkmail_failed').html(data.total_failed +' Failed');
    }
    createOrUpdateChart({
        labels: [],
        sent: [],
        failed: [],
        total_mail:0,
        total_sent:0,
        total_failed:0,
    });

    function checmail_fetch_chart_date(day=7) {
        fetch(ajaxurl + '?action=get_email_analytics&ck_mail_security_nonce='+ck_mail_security_nonce+'&ck_days='+day)
        .then((response) => response.json())
        .then((data) => {
            createOrUpdateChart(data);
        })
        .catch((error) => {
            console.error("Error fetching chart data:", error);
        });
    }

    
    jQuery('#checkmail-dashboard-date-range').change(function () {
        var days = jQuery(this).val();
        checmail_fetch_chart_date(days);
    });
    checmail_fetch_chart_date(7)

});
