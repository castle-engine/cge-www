(function(){var isIe=/(trident|msie)/i.test(navigator.userAgent);if(isIe&&document.getElementById&&window.addEventListener){window.addEventListener('hashchange',function(){var id=location.hash.substring(1),element;if(!(/^[A-z0-9_-]+$/.test(id))){return;}
element=document.getElementById(id);if(element){if(!(/^(?:a|select|input|button|textarea)$/i.test(element.tagName))){element.tabIndex=-1;}
element.focus();}},false);}})();;(function($){var $body=$('body'),$customHeader=$body.find('.custom-header'),$branding=$customHeader.find('.site-branding'),$navigation=$body.find('.navigation-top'),$navWrap=$navigation.find('.wrap'),$navMenuItem=$navigation.find('.menu-item'),$menuToggle=$navigation.find('.menu-toggle'),$menuScrollDown=$body.find('.menu-scroll-down'),$sidebar=$body.find('#secondary'),$entryContent=$body.find('.entry-content'),$formatQuote=$body.find('.format-quote blockquote'),isFrontPage=$body.hasClass('twentyseventeen-front-page')||$body.hasClass('home blog'),navigationFixedClass='site-navigation-fixed',navigationHeight,navigationOuterHeight,navPadding,navMenuItemHeight,idealNavHeight,navIsNotTooTall,headerOffset,menuTop=0,resizeTimer;$('a[href], area[href], input:not([disabled]), select:not([disabled]), textarea:not([disabled]), button:not([disabled]), iframe, object, embed, [tabindex], [contenteditable]','.site-content-contain').filter(':visible').on('focus',function(){if($navigation.hasClass('site-navigation-fixed')){var windowScrollTop=$(window).scrollTop(),fixedNavHeight=$navigation.height(),itemScrollTop=$(this).offset().top,offsetDiff=itemScrollTop-windowScrollTop;if($('#wpadminbar').length){offsetDiff-=$('#wpadminbar').height();}
if(offsetDiff<fixedNavHeight){$(window).scrollTo(itemScrollTop-(fixedNavHeight+50),0);}}});function setNavProps(){navigationHeight=$navigation.height();navigationOuterHeight=$navigation.outerHeight();navPadding=parseFloat($navWrap.css('padding-top'))*2;navMenuItemHeight=$navMenuItem.outerHeight()*2;idealNavHeight=navPadding+navMenuItemHeight;navIsNotTooTall=navigationHeight<=idealNavHeight;}
function adjustScrollClass(){if('none'===$menuToggle.css('display')){if(navIsNotTooTall){if(isFrontPage&&($body.hasClass('has-header-image')||$body.hasClass('has-header-video'))){headerOffset=$customHeader.innerHeight()-navigationOuterHeight;}else{headerOffset=$customHeader.innerHeight();}
if($(window).scrollTop()>=headerOffset){$navigation.addClass(navigationFixedClass);}else{$navigation.removeClass(navigationFixedClass);}}else{$navigation.removeClass(navigationFixedClass);}}}
function adjustHeaderHeight(){if('none'===$menuToggle.css('display')){if(isFrontPage){$branding.css('margin-bottom',navigationOuterHeight);}else{$customHeader.css('margin-bottom',navigationOuterHeight);}}else{$customHeader.css('margin-bottom','0');$branding.css('margin-bottom','0');}}
function setQuotesIcon(){$(twentyseventeenScreenReaderText.quote).prependTo($formatQuote);}
function belowEntryMetaClass(param){var sidebarPos,sidebarPosBottom;if(!$body.hasClass('has-sidebar')||typeof $sidebar==='undefined'||$sidebar.length<1||($body.hasClass('search')||$body.hasClass('single-attachment')||$body.hasClass('error404')||$body.hasClass('twentyseventeen-front-page'))){return;}
sidebarPos=$sidebar.offset();sidebarPosBottom=sidebarPos.top+($sidebar.height()+28);$entryContent.find(param).each(function(){var $element=$(this),elementPos=$element.offset(),elementPosTop=elementPos.top;if(elementPosTop>sidebarPosBottom){$element.addClass('below-entry-meta');}else{$element.removeClass('below-entry-meta');}});}
function supportsInlineSVG(){var div=document.createElement('div');div.innerHTML='<svg/>';return'http://www.w3.org/2000/svg'===('undefined'!==typeof SVGRect&&div.firstChild&&div.firstChild.namespaceURI);}
function checkiOS(){return /iPad|iPhone|iPod/.test(navigator.userAgent)&&!window.MSStream;}
function supportsFixedBackground(){var el=document.createElement('div'),isSupported;try{if(!('backgroundAttachment'in el.style)||checkiOS()){return false;}
el.style.backgroundAttachment='fixed';isSupported=('fixed'===el.style.backgroundAttachment);return isSupported;}
catch(e){return false;}}
$(function(){if($navigation.length){setNavProps();adjustScrollClass();}
if($menuScrollDown.length){if($('body').hasClass('admin-bar')){menuTop-=32;}
if($('body').hasClass('blog')){menuTop-=30;}
if(!$navigation.length){navigationOuterHeight=0;}
$menuScrollDown.on('click',function(e){e.preventDefault();$(window).scrollTo('#primary',{duration:600,offset:{top:menuTop-navigationOuterHeight}});});}
adjustHeaderHeight();setQuotesIcon();belowEntryMetaClass('blockquote.alignleft, blockquote.alignright');if(true===supportsInlineSVG()){document.documentElement.className=document.documentElement.className.replace(/(\s*)no-svg(\s*)/,'$1svg$2');}
if(true===supportsFixedBackground()){document.documentElement.className+=' background-fixed';}});if($navigation.length){$(window).on('scroll',function(){adjustScrollClass();adjustHeaderHeight();});$(window).on('resize',function(){setNavProps();setTimeout(adjustScrollClass,500);});}
$(window).on('resize',function(){clearTimeout(resizeTimer);resizeTimer=setTimeout(function(){belowEntryMetaClass('blockquote.alignleft, blockquote.alignright');},300);setTimeout(adjustHeaderHeight,1000);});$(document).on('wp-custom-header-video-loaded',function(){$body.addClass('has-header-video');});})(jQuery);;
/*!
 * jQuery.scrollTo
 * Copyright (c) 2007 Ariel Flesler - aflesler ○ gmail • com | https://github.com/flesler
 * Licensed under MIT
 * https://github.com/flesler/jquery.scrollTo
 * @projectDescription Lightweight, cross-browser and highly customizable animated scrolling with jQuery
 * @author Ariel Flesler
 * @version 2.1.3
 */
;(function(factory){'use strict';if(typeof define==='function'&&define.amd){define(['jquery'],factory);}else if(typeof module!=='undefined'&&module.exports){module.exports=factory(require('jquery'));}else{factory(jQuery);}})(function($){'use strict';var $scrollTo=$.scrollTo=function(target,duration,settings){return $(window).scrollTo(target,duration,settings);};$scrollTo.defaults={axis:'xy',duration:0,limit:true};function isWin(elem){return!elem.nodeName||$.inArray(elem.nodeName.toLowerCase(),['iframe','#document','html','body'])!==-1;}
function isFunction(obj){return typeof obj==='function'}
$.fn.scrollTo=function(target,duration,settings){if(typeof duration==='object'){settings=duration;duration=0;}
if(typeof settings==='function'){settings={onAfter:settings};}
if(target==='max'){target=9e9;}
settings=$.extend({},$scrollTo.defaults,settings);duration=duration||settings.duration;var queue=settings.queue&&settings.axis.length>1;if(queue){duration/=2;}
settings.offset=both(settings.offset);settings.over=both(settings.over);return this.each(function(){if(target===null)return;var win=isWin(this),elem=win?this.contentWindow||window:this,$elem=$(elem),targ=target,attr={},toff;switch(typeof targ){case'number':case'string':if(/^([+-]=?)?\d+(\.\d+)?(px|%)?$/.test(targ)){targ=both(targ);break;}
targ=win?$(targ):$(targ,elem);case'object':if(targ.length===0)return;if(targ.is||targ.style){toff=(targ=$(targ)).offset();}}
var offset=isFunction(settings.offset)&&settings.offset(elem,targ)||settings.offset;$.each(settings.axis.split(''),function(i,axis){var Pos=axis==='x'?'Left':'Top',pos=Pos.toLowerCase(),key='scroll'+Pos,prev=$elem[key](),max=$scrollTo.max(elem,axis);if(toff){attr[key]=toff[pos]+(win?0:prev-$elem.offset()[pos]);if(settings.margin){attr[key]-=parseInt(targ.css('margin'+Pos),10)||0;attr[key]-=parseInt(targ.css('border'+Pos+'Width'),10)||0;}
attr[key]+=offset[pos]||0;if(settings.over[pos]){attr[key]+=targ[axis==='x'?'width':'height']()*settings.over[pos];}}else{var val=targ[pos];attr[key]=val.slice&&val.slice(-1)==='%'?parseFloat(val)/100*max:val;}
if(settings.limit&&/^\d+$/.test(attr[key])){attr[key]=attr[key]<=0?0:Math.min(attr[key],max);}
if(!i&&settings.axis.length>1){if(prev===attr[key]){attr={};}else if(queue){animate(settings.onAfterFirst);attr={};}}});animate(settings.onAfter);function animate(callback){var opts=$.extend({},settings,{queue:true,duration:duration,complete:callback&&function(){callback.call(elem,targ,settings);}});$elem.animate(attr,opts);}});};$scrollTo.max=function(elem,axis){var Dim=axis==='x'?'Width':'Height',scroll='scroll'+Dim;if(!isWin(elem))
return elem[scroll]-$(elem)[Dim.toLowerCase()]();var size='client'+Dim,doc=elem.ownerDocument||elem.document,html=doc.documentElement,body=doc.body;return Math.max(html[scroll],body[scroll])-Math.min(html[size],body[size]);};function both(val){return isFunction(val)||$.isPlainObject(val)?val:{top:val,left:val};}
$.Tween.propHooks.scrollLeft=$.Tween.propHooks.scrollTop={get:function(t){return $(t.elem)[t.prop]();},set:function(t){var curr=this.get(t);if(t.options.interrupt&&t._last&&t._last!==curr){return $(t.elem).stop();}
var next=Math.round(t.now);if(curr!==next){$(t.elem)[t.prop](next);t._last=this.get(t);}}};return $scrollTo;});