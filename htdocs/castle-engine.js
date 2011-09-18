/* Based on http://www.netlobo.com/javascript_get_element_id.html */
function kambi_get_element_by_id(id)
{
  if (document.getElementById)
    return document.getElementById(id); else
  if (document.all)
    return document.all[id]; else
  if (document.layers)
    return document.layers[id];
}

function kambi_toggle_display(id)
{
  var element = kambi_get_element_by_id(id);
  if (element.style.display == "none")
    element.style.display=""; else
    element.style.display="none";
}
