(function () {
    var u, p, e, t, n, a, d

    function i() {}

    function o(e) {
        this.path = e, this.at_2x_path = e.replace(/\.\w+$/, function (e) {
            return "@2x" + e
        })
    }

    function s(e) {
        this.el = e, this.path = new o(this.el.getAttribute("src"))
        var t = this
        this.path.check_2x_variant(function (e) {
            e && t.swap()
        })
    }

    function r() {
        mq_tag = window.getComputedStyle(document.body, ":after").getPropertyValue("content"), -1 != mq_tag.indexOf("drawer_navigation") ? $("#site-map-link").click(function () {
            return $("body").toggleClass("show-sidemenu"), !1
        }) : $("#site-map-link").click(function () {
            return $("body, html").animate({
                scrollTop: $("#site-map").offset().top
            }, 500), !1
        }), u || (-1 != mq_tag.indexOf("load_supernavs") && !supernavs_loaded || e) && ($.get("/box/supernav-lfe-about/", function (e) {
            $(".lfe-navigation li#about .subnav").append(e)
        }, "html"), $("li#about").addClass("with-supernav"), $.get("/box/supernav-lfe-downloads/", function (e) {
            $("li#downloads .subnav").append(e), -1 != navigator.appVersion.indexOf("Win") && ($(".download-unknown").hide(), $(".download-os-windows").show()), -1 != navigator.appVersion.indexOf("Mac") && ($(".download-unknown").hide(), $(".download-os-mac-osx").show()), -1 != navigator.appVersion.indexOf("X11") && ($(".download-unknown").hide(), $(".download-os-source").show()), -1 != navigator.appVersion.indexOf("Linux") && ($(".download-unknown").hide(), $(".download-os-source").show())
        }, "html"), $("li#downloads").addClass("with-supernav"), $.get("/box/supernav-lfe-documentation/", function (e) {
            $("li#documentation .subnav").append(e)
        }, "html"), $("li#documentation").addClass("with-supernav"), $.get("/box/supernav-lfe-community/", function (e) {
            $("li#community .subnav").append(e)
        }, "html"), $("li#community").addClass("with-supernav"), $.get("/box/supernav-lfe-success-stories/", function (e) {
            $("li#success-stories .subnav").append(e)
        }, "html"), $("li#success-stories").addClass("with-supernav"), $.get("/box/supernav-lfe-blog/", function (e) {
            $("li#blog .subnav").append(e)
        }, "html"), $("li#blog").addClass("with-supernav"), $.get("/box/supernav-lfe-events/", function (e) {
            $("li#events .subnav").append(e)
        }, "html"), $("li#events").addClass("with-supernav"), supernavs_loaded = !0)
    }
    window.log = function () {
            var e, t
            log.history = log.history || [], log.history.push(arguments), this.console && ((e = arguments).callee = e.callee.caller, t = [].slice.call(e), "object" == typeof console.log ? log.apply.call(console.log, console, t) : console.log.apply(console, t))
        },
        function (e) {
            function t() {}
            for (var n, i = "assert,count,debug,dir,dirxml,error,exception,group,groupCollapsed,groupEnd,info,log,markTimeline,profile,profileEnd,time,timeEnd,trace,warn".split(","); n = i.pop();) e[n] = e[n] || t
        }(function () {
            try {
                return console.log(), window.console
            } catch (e) {
                return window.console = {}
            }
        }()),
        /*!
         * jQuery Cookie Plugin v1.3.1
         * https://github.com/carhartl/jquery-cookie
         *
         * Copyright 2013 Klaus Hartl
         * Released under the MIT license
         */
        t = function (m) {
            var h, t = /\+/g

            function v(e) {
                return e
            }

            function f(e) {
                return decodeURIComponent(e.replace(t, " "))
            }

            function g(e) {
                0 === e.indexOf('"') && (e = e.slice(1, -1).replace(/\\"/g, '"').replace(/\\\\/g, "\\"))
                try {
                    return h.json ? JSON.parse(e) : e
                } catch (e) {}
            }(h = m.cookie = function (e, t, n) {
                var i, a, o, s, r, l, c, d, u, p
                if (void 0 !== t) return "number" == typeof (n = m.extend({}, h.defaults, n)).expires && (i = n.expires, (a = n.expires = new Date).setDate(a.getDate() + i)), t = h.json ? JSON.stringify(t) : String(t), document.cookie = [encodeURIComponent(e), "=", h.raw ? t : encodeURIComponent(t), n.expires ? "; expires=" + n.expires.toUTCString() : "", n.path ? "; path=" + n.path : "", n.domain ? "; domain=" + n.domain : "", n.secure ? "; secure" : ""].join("")
                for (o = h.raw ? v : f, r = e ? void 0 : {}, l = 0, c = (s = document.cookie.split("; ")).length; l < c; l++) {
                    if (u = o((d = s[l].split("=")).shift()), p = o(d.join("=")), e && e === u) {
                        r = g(p)
                        break
                    }
                    e || (r[u] = g(p))
                }
                return r
            }).defaults = {}, m.removeCookie = function (e, t) {
                return void 0 !== m.cookie(e) && (m.cookie(e, "", m.extend(t, {
                    expires: -1
                })), !0)
            }
        }, "function" == typeof define && define.amd && define.amd.jQuery ? define(["jquery"], t) : t(jQuery),
        /*! Retina.js
         * https://github.com/imulus/retinajs/blob/master/src/retina.js
         * Copyright (C) 2012 Ben Atkin
         * MIT License.
         */
        n = "undefined" == typeof exports ? window : exports, a = {
            check_mime_type: !0
        }, (n.Retina = i).configure = function (e) {
            for (var t in null === e && (e = {}), e) a[t] = e[t]
        }, i.init = function (e) {
            null === e && (e = n)
            var a = e.onload || new Function
            e.onload = function () {
                for (var e, t = document.getElementsByTagName("img"), n = [], i = 0; i < t.length; i++) e = t[i], n.push(new s(e))
                a()
            }
        }, i.isRetina = function () {
            return 1 < n.devicePixelRatio || !(!n.matchMedia || !n.matchMedia("(-webkit-min-device-pixel-ratio: 1.5),                      (min--moz-device-pixel-ratio: 1.5),                      (-o-min-device-pixel-ratio: 3/2),                      (min-resolution: 1.5dppx)").matches)
        }, (n.RetinaImagePath = o).confirmed_paths = [], o.prototype.is_external = function () {
            return !(!this.path.match(/^https?\:/i) || this.path.match("//" + document.domain))
        }, o.prototype.check_2x_variant = function (t) {
            var n, i = this
            return this.is_external() ? t(!1) : this.at_2x_path in o.confirmed_paths ? t(!0) : ((n = new XMLHttpRequest).open("HEAD", this.at_2x_path), n.onreadystatechange = function () {
                if (4 != n.readyState) return t(!1)
                if (200 <= n.status && n.status <= 399) {
                    if (a.check_mime_type) {
                        var e = n.getResponseHeader("Content-Type")
                        if (null === e || !e.match(/^image/i)) return t(!1)
                    }
                    return o.confirmed_paths.push(i.at_2x_path), t(!0)
                }
                return t(!1)
            }, void n.send())
        }, (n.RetinaImage = s).prototype.swap = function (t) {
            void 0 === t && (t = this.path.at_2x_path)
            var n = this! function e() {
                n.el.complete ? (n.el.setAttribute("width", n.el.offsetWidth), n.el.setAttribute("height", n.el.offsetHeight), n.el.setAttribute("src", t)) : setTimeout(e, 5)
            }()
        }, i.isRetina() && i.init(n), (d = jQuery).flexslider = function (u, e) {
            var p = d(u),
                m = d.extend({}, d.flexslider.defaults, e),
                s = m.namespace,
                r = "ontouchstart" in window || window.DocumentTouch && document instanceof DocumentTouch,
                i = r ? "touchend" : "click",
                h = "vertical" === m.direction,
                v = m.reverse,
                f = 0 < m.itemWidth,
                g = "fade" === m.animation,
                l = "" !== m.asNavFor,
                c = {}
            d.data(u, "flexslider", p), c = {
                init: function () {
                    var e, t, n, i
                    if (p.animating = !1, p.currentSlide = m.startAt, p.animatingTo = p.currentSlide, p.atEnd = 0 === p.currentSlide || p.currentSlide === p.last, p.containerSelector = m.selector.substr(0, m.selector.search(" ")), p.slides = d(m.selector, p), p.container = d(p.containerSelector, p), p.count = p.slides.length, p.syncExists = 0 < d(m.sync).length, "slide" === m.animation && (m.animation = "swing"), p.prop = h ? "top" : "marginLeft", p.args = {}, p.manualPause = !1, e = p, (t = !m.video) && (t = !g) && (t = m.useCSS)) e: {
                        for (i in t = document.createElement("div"), n = ["perspectiveProperty", "WebkitPerspective", "MozPerspective", "OPerspective", "msPerspective"])
                            if (void 0 !== t.style[n[i]]) {
                                p.pfx = n[i].replace("Perspective", "").toLowerCase(), p.prop = "-" + p.pfx + "-transform", t = !0
                                break e
                            } t = !1
                    }
                    e.transitions = t, "" !== m.controlsContainer && (p.controlsContainer = 0 < d(m.controlsContainer).length && d(m.controlsContainer)), "" !== m.manualControls && (p.manualControls = 0 < d(m.manualControls).length && d(m.manualControls)), m.randomize && (p.slides.sort(function () {
                        return Math.round(Math.random()) - .5
                    }), p.container.empty().append(p.slides)), p.doMath(), l && c.asNav.setup(), p.setup("init"), m.controlNav && c.controlNav.setup(), m.directionNav && c.directionNav.setup(), m.keyboard && (1 === d(p.containerSelector).length || m.multipleKeyboard) && d(document).bind("keyup", function (e) {
                        e = e.keyCode, p.animating || 39 !== e && 37 !== e || (e = 39 === e ? p.getTarget("next") : 37 === e && p.getTarget("prev"), p.flexAnimate(e, m.pauseOnAction))
                    }), m.mousewheel && p.bind("mousewheel", function (e, t) {
                        e.preventDefault()
                        var n = t < 0 ? p.getTarget("next") : p.getTarget("prev")
                        p.flexAnimate(n, m.pauseOnAction)
                    }), m.pausePlay && c.pausePlay.setup(), m.slideshow && (m.pauseOnHover && p.hover(function () {
                        p.manualPlay || p.manualPause || p.pause()
                    }, function () {
                        p.manualPause || p.manualPlay || p.play()
                    }), 0 < m.initDelay ? setTimeout(p.play, m.initDelay) : p.play()), r && m.touch && c.touch(), (!g || g && m.smoothHeight) && d(window).bind("resize focus", c.resize), setTimeout(function () {
                        m.start(p)
                    }, 200)
                },
                asNav: {
                    setup: function () {
                        p.asNav = !0, p.animatingTo = Math.floor(p.currentSlide / p.move), p.currentItem = p.currentSlide, p.slides.removeClass(s + "active-slide").eq(p.currentItem).addClass(s + "active-slide"), p.slides.click(function (e) {
                            e.preventDefault()
                            var t = (e = d(this)).index()
                            d(m.asNavFor).data("flexslider").animating || e.hasClass("active") || (p.direction = p.currentItem < t ? "next" : "prev", p.flexAnimate(t, m.pauseOnAction, !1, !0, !0))
                        })
                    }
                },
                controlNav: {
                    setup: function () {
                        p.manualControls ? c.controlNav.setupManual() : c.controlNav.setupPaging()
                    },
                    setupPaging: function () {
                        var e, t, n = 1
                        if (p.controlNavScaffold = d('<ol class="' + s + "control-nav " + s + ("thumbnails" === m.controlNav ? "control-thumbs" : "control-paging") + '"></ol>'), 1 < p.pagingCount)
                            for (t = 0; t < p.pagingCount; t++) e = "thumbnails" === m.controlNav ? '<img src="' + p.slides.eq(t).attr("data-thumb") + '"/>' : "<a>" + n + "</a>", p.controlNavScaffold.append("<li>" + e + "</li>"), n++
                        p.controlsContainer ? d(p.controlsContainer).append(p.controlNavScaffold) : p.append(p.controlNavScaffold), c.controlNav.set(), c.controlNav.active(), p.controlNavScaffold.delegate("a, img", i, function (e) {
                            e.preventDefault(), e = d(this)
                            var t = p.controlNav.index(e)
                            e.hasClass(s + "active") || (p.direction = t > p.currentSlide ? "next" : "prev", p.flexAnimate(t, m.pauseOnAction))
                        }), r && p.controlNavScaffold.delegate("a", "click touchstart", function (e) {
                            e.preventDefault()
                        })
                    },
                    setupManual: function () {
                        p.controlNav = p.manualControls, c.controlNav.active(), p.controlNav.live(i, function (e) {
                            e.preventDefault(), e = d(this)
                            var t = p.controlNav.index(e)
                            e.hasClass(s + "active") || (t > p.currentSlide ? p.direction = "next" : p.direction = "prev", p.flexAnimate(t, m.pauseOnAction))
                        }), r && p.controlNav.live("click touchstart", function (e) {
                            e.preventDefault()
                        })
                    },
                    set: function () {
                        p.controlNav = d("." + s + "control-nav li " + ("thumbnails" === m.controlNav ? "img" : "a"), p.controlsContainer ? p.controlsContainer : p)
                    },
                    active: function () {
                        p.controlNav.removeClass(s + "active").eq(p.animatingTo).addClass(s + "active")
                    },
                    update: function (e, t) {
                        1 < p.pagingCount && "add" === e ? p.controlNavScaffold.append(d("<li><a>" + p.count + "</a></li>")) : 1 === p.pagingCount ? p.controlNavScaffold.find("li").remove() : p.controlNav.eq(t).closest("li").remove(), c.controlNav.set(), 1 < p.pagingCount && p.pagingCount !== p.controlNav.length ? p.update(t, e) : c.controlNav.active()
                    }
                },
                directionNav: {
                    setup: function () {
                        var e = d('<ul class="' + s + 'direction-nav"><li><a class="' + s + 'prev" href="#">' + m.prevText + '</a></li><li><a class="' + s + 'next" href="#">' + m.nextText + "</a></li></ul>")
                        p.controlsContainer ? (d(p.controlsContainer).append(e), p.directionNav = d("." + s + "direction-nav li a", p.controlsContainer)) : (p.append(e), p.directionNav = d("." + s + "direction-nav li a", p)), c.directionNav.update(), p.directionNav.bind(i, function (e) {
                            e.preventDefault(), e = d(this).hasClass(s + "next") ? p.getTarget("next") : p.getTarget("prev"), p.flexAnimate(e, m.pauseOnAction)
                        }), r && p.directionNav.bind("click touchstart", function (e) {
                            e.preventDefault()
                        })
                    },
                    update: function () {
                        var e = s + "disabled"
                        1 === p.pagingCount ? p.directionNav.addClass(e) : m.animationLoop ? p.directionNav.removeClass(e) : 0 === p.animatingTo ? p.directionNav.removeClass(e).filter("." + s + "prev").addClass(e) : p.animatingTo === p.last ? p.directionNav.removeClass(e).filter("." + s + "next").addClass(e) : p.directionNav.removeClass(e)
                    }
                },
                pausePlay: {
                    setup: function () {
                        var e = d('<div class="' + s + 'pauseplay"><a></a></div>')
                        p.controlsContainer ? (p.controlsContainer.append(e), p.pausePlay = d("." + s + "pauseplay a", p.controlsContainer)) : (p.append(e), p.pausePlay = d("." + s + "pauseplay a", p)), c.pausePlay.update(m.slideshow ? s + "pause" : s + "play"), p.pausePlay.bind(i, function (e) {
                            e.preventDefault(), d(this).hasClass(s + "pause") ? (p.manualPause = !0, p.manualPlay = !1, p.pause()) : (p.manualPause = !1, p.manualPlay = !0, p.play())
                        }), r && p.pausePlay.bind("click touchstart", function (e) {
                            e.preventDefault()
                        })
                    },
                    update: function (e) {
                        "play" === e ? p.pausePlay.removeClass(s + "pause").addClass(s + "play").text(m.playText) : p.pausePlay.removeClass(s + "play").addClass(s + "pause").text(m.pauseText)
                    }
                },
                touch: function () {
                    function n(e) {
                        l = h ? a - e.touches[0].pageY : a - e.touches[0].pageX, (!(d = h ? Math.abs(l) < Math.abs(e.touches[0].pageX - o) : Math.abs(l) < Math.abs(e.touches[0].pageY - o)) || 500 < Number(new Date) - c) && (e.preventDefault(), !g && p.transitions && (m.animationLoop || (l /= 0 === p.currentSlide && l < 0 || p.currentSlide === p.last && 0 < l ? Math.abs(l) / r + 2 : 1), p.setProps(s + l, "setTouch")))
                    }

                    function i() {
                        var e, t
                        u.removeEventListener("touchmove", n, !1), p.animatingTo !== p.currentSlide || d || null === l || (t = 0 < (e = v ? -l : l) ? p.getTarget("next") : p.getTarget("prev"), p.canAdvance(t) && (Number(new Date) - c < 550 && 50 < Math.abs(e) || Math.abs(e) > r / 2) ? p.flexAnimate(t, m.pauseOnAction) : g || p.flexAnimate(p.currentSlide, m.pauseOnAction, !0)), u.removeEventListener("touchend", i, !1), s = l = o = a = null
                    }
                    var a, o, s, r, l, c, d = !1
                    u.addEventListener("touchstart", function (e) {
                        p.animating ? e.preventDefault() : 1 === e.touches.length && (p.pause(), r = h ? p.h : p.w, c = Number(new Date), s = f && v && p.animatingTo === p.last ? 0 : f && v ? p.limit - (p.itemW + m.itemMargin) * p.move * p.animatingTo : f && p.currentSlide === p.last ? p.limit : f ? (p.itemW + m.itemMargin) * p.move * p.currentSlide : v ? (p.last - p.currentSlide + p.cloneOffset) * r : (p.currentSlide + p.cloneOffset) * r, a = h ? e.touches[0].pageY : e.touches[0].pageX, o = h ? e.touches[0].pageX : e.touches[0].pageY, u.addEventListener("touchmove", n, !1), u.addEventListener("touchend", i, !1))
                    }, !1)
                },
                resize: function () {
                    !p.animating && p.is(":visible") && (f || p.doMath(), g ? c.smoothHeight() : f ? (p.slides.width(p.computedW), p.update(p.pagingCount), p.setProps()) : h ? (p.viewport.height(p.h), p.setProps(p.h, "setTotal")) : (m.smoothHeight && c.smoothHeight(), p.newSlides.width(p.computedW), p.setProps(p.computedW, "setTotal")))
                },
                smoothHeight: function (e) {
                    var t
                    h && !g || (t = g ? p : p.viewport, e ? t.animate({
                        height: p.slides.eq(p.animatingTo).height()
                    }, e) : t.height(p.slides.eq(p.animatingTo).height()))
                },
                sync: function (e) {
                    var t = d(m.sync).data("flexslider"),
                        n = p.animatingTo
                    switch (e) {
                        case "animate":
                            t.flexAnimate(n, m.pauseOnAction, !1, !0)
                            break
                        case "play":
                            t.playing || t.asNav || t.play()
                            break
                        case "pause":
                            t.pause()
                    }
                }
            }, p.flexAnimate = function (e, t, n, i, a) {
                if (l && 1 === p.pagingCount && (p.direction = p.currentItem < e ? "next" : "prev"), !p.animating && (p.canAdvance(e, a) || n) && p.is(":visible")) {
                    if (l && i) {
                        if (n = d(m.asNavFor).data("flexslider"), p.atEnd = 0 === e || e === p.count - 1, n.flexAnimate(e, !0, !1, !0, a), p.direction = p.currentItem < e ? "next" : "prev", n.direction = p.direction, Math.ceil((e + 1) / p.visible) - 1 === p.currentSlide || 0 === e) return p.currentItem = e, p.slides.removeClass(s + "active-slide").eq(e).addClass(s + "active-slide"), !1
                        p.currentItem = e, p.slides.removeClass(s + "active-slide").eq(e).addClass(s + "active-slide"), e = Math.floor(e / p.visible)
                    }
                    var o
                    p.animating = !0, p.animatingTo = e, m.before(p), t && p.pause(), p.syncExists && !a && c.sync("animate"), m.controlNav && c.controlNav.active(), f || p.slides.removeClass(s + "active-slide").eq(e).addClass(s + "active-slide"), p.atEnd = 0 === e || e === p.last, m.directionNav && c.directionNav.update(), e === p.last && (m.end(p), m.animationLoop || p.pause()), g ? r ? (p.slides.eq(p.currentSlide).css({
                        opacity: 0,
                        zIndex: 1
                    }), p.slides.eq(e).css({
                        opacity: 1,
                        zIndex: 2
                    }), p.slides.unbind("webkitTransitionEnd transitionend"), p.slides.eq(p.currentSlide).bind("webkitTransitionEnd transitionend", function () {
                        m.after(p)
                    }), p.animating = !1, p.currentSlide = p.animatingTo) : (p.slides.eq(p.currentSlide).fadeOut(m.animationSpeed, m.easing), p.slides.eq(e).fadeIn(m.animationSpeed, m.easing, p.wrapup)) : (o = h ? p.slides.filter(":first").height() : p.computedW, e = f ? (e = m.itemWidth > p.w ? 2 * m.itemMargin : m.itemMargin, (e = (p.itemW + e) * p.move * p.animatingTo) > p.limit && 1 !== p.visible ? p.limit : e) : 0 === p.currentSlide && e === p.count - 1 && m.animationLoop && "next" !== p.direction ? v ? (p.count + p.cloneOffset) * o : 0 : p.currentSlide === p.last && 0 === e && m.animationLoop && "prev" !== p.direction ? v ? 0 : (p.count + 1) * o : v ? (p.count - 1 - e + p.cloneOffset) * o : (e + p.cloneOffset) * o, p.setProps(e, "", m.animationSpeed), p.transitions ? (m.animationLoop && p.atEnd || (p.animating = !1, p.currentSlide = p.animatingTo), p.container.unbind("webkitTransitionEnd transitionend"), p.container.bind("webkitTransitionEnd transitionend", function () {
                        p.wrapup(o)
                    })) : p.container.animate(p.args, m.animationSpeed, m.easing, function () {
                        p.wrapup(o)
                    })), m.smoothHeight && c.smoothHeight(m.animationSpeed)
                }
            }, p.wrapup = function (e) {
                g || f || (0 === p.currentSlide && p.animatingTo === p.last && m.animationLoop ? p.setProps(e, "jumpEnd") : p.currentSlide === p.last && 0 === p.animatingTo && m.animationLoop && p.setProps(e, "jumpStart")), p.animating = !1, p.currentSlide = p.animatingTo, m.after(p)
            }, p.animateSlides = function () {
                p.animating || p.flexAnimate(p.getTarget("next"))
            }, p.pause = function () {
                clearInterval(p.animatedSlides), p.playing = !1, m.pausePlay && c.pausePlay.update("play"), p.syncExists && c.sync("pause")
            }, p.play = function () {
                p.animatedSlides = setInterval(p.animateSlides, m.slideshowSpeed), p.playing = !0, m.pausePlay && c.pausePlay.update("pause"), p.syncExists && c.sync("play")
            }, p.canAdvance = function (e, t) {
                var n = l ? p.pagingCount - 1 : p.last
                return !!t || (l && p.currentItem === p.count - 1 && 0 === e && "prev" === p.direction || (!l || 0 !== p.currentItem || e !== p.pagingCount - 1 || "next" === p.direction) && ((e !== p.currentSlide || l) && (!!m.animationLoop || (!p.atEnd || 0 !== p.currentSlide || e !== n || "next" === p.direction) && (!p.atEnd || p.currentSlide !== n || 0 !== e || "next" !== p.direction))))
            }, p.getTarget = function (e) {
                return "next" === (p.direction = e) ? p.currentSlide === p.last ? 0 : p.currentSlide + 1 : 0 === p.currentSlide ? p.last : p.currentSlide - 1
            }, p.setProps = function (e, t, n) {
                var i = e || (p.itemW + m.itemMargin) * p.move * p.animatingTo,
                    a = -1 * function () {
                        if (f) return "setTouch" === t ? e : v && p.animatingTo === p.last ? 0 : v ? p.limit - (p.itemW + m.itemMargin) * p.move * p.animatingTo : p.animatingTo === p.last ? p.limit : i
                        switch (t) {
                            case "setTotal":
                                return v ? (p.count - 1 - p.currentSlide + p.cloneOffset) * e : (p.currentSlide + p.cloneOffset) * e
                            case "setTouch":
                                return e
                            case "jumpEnd":
                                return v ? e : p.count * e
                            case "jumpStart":
                                return v ? p.count * e : e
                            default:
                                return e
                        }
                    }() + "px"
                p.transitions && (a = h ? "translate3d(0," + a + ",0)" : "translate3d(" + a + ",0,0)", n = void 0 !== n ? n / 1e3 + "s" : "0s", p.container.css("-" + p.pfx + "-transition-duration", n)), p.args[p.prop] = a, !p.transitions && void 0 !== n || p.container.css(p.args)
            }, p.setup = function (e) {
                var t, n
                g ? (p.slides.css({
                    width: "100%",
                    float: "left",
                    marginRight: "-100%",
                    position: "relative"
                }), "init" === e && (r ? p.slides.css({
                    opacity: 0,
                    display: "block",
                    webkitTransition: "opacity " + m.animationSpeed / 1e3 + "s ease",
                    zIndex: 1
                }).eq(p.currentSlide).css({
                    opacity: 1,
                    zIndex: 2
                }) : p.slides.eq(p.currentSlide).fadeIn(m.animationSpeed, m.easing)), m.smoothHeight && c.smoothHeight()) : ("init" === e && (p.viewport = d('<div class="' + s + 'viewport"></div>').css({
                    overflow: "hidden",
                    position: "relative"
                }).appendTo(p).append(p.container), p.cloneCount = 0, p.cloneOffset = 0, v && (n = d.makeArray(p.slides).reverse(), p.slides = d(n), p.container.empty().append(p.slides))), m.animationLoop && !f && (p.cloneCount = 2, p.cloneOffset = 1, "init" !== e && p.container.find(".clone").remove(), p.container.append(p.slides.first().clone().addClass("clone")).prepend(p.slides.last().clone().addClass("clone"))), p.newSlides = d(m.selector, p), t = v ? p.count - 1 - p.currentSlide + p.cloneOffset : p.currentSlide + p.cloneOffset, h && !f ? (p.container.height(200 * (p.count + p.cloneCount) + "%").css("position", "absolute").width("100%"), setTimeout(function () {
                    p.newSlides.css({
                        display: "block"
                    }), p.doMath(), p.viewport.height(p.h), p.setProps(t * p.h, "init")
                }, "init" === e ? 100 : 0)) : (p.container.width(200 * (p.count + p.cloneCount) + "%"), p.setProps(t * p.computedW, "init"), setTimeout(function () {
                    p.doMath(), p.newSlides.css({
                        width: p.computedW,
                        float: "left",
                        display: "block"
                    }), m.smoothHeight && c.smoothHeight()
                }, "init" === e ? 100 : 0))), f || p.slides.removeClass(s + "active-slide").eq(p.currentSlide).addClass(s + "active-slide")
            }, p.doMath = function () {
                var e = p.slides.first(),
                    t = m.itemMargin,
                    n = m.minItems,
                    i = m.maxItems
                p.w = p.width(), p.h = e.height(), p.boxPadding = e.outerWidth() - e.width(), f ? (p.itemT = m.itemWidth + t, p.minW = n ? n * p.itemT : p.w, p.maxW = i ? i * p.itemT : p.w, p.itemW = p.minW > p.w ? (p.w - t * n) / n : p.maxW < p.w ? (p.w - t * i) / i : m.itemWidth > p.w ? p.w : m.itemWidth, p.visible = Math.floor(p.w / (p.itemW + t)), p.move = 0 < m.move && m.move < p.visible ? m.move : p.visible, p.pagingCount = Math.ceil((p.count - p.visible) / p.move + 1), p.last = p.pagingCount - 1, p.limit = 1 === p.pagingCount ? 0 : m.itemWidth > p.w ? (p.itemW + 2 * t) * p.count - p.w - t : (p.itemW + t) * p.count - p.w - t) : (p.itemW = p.w, p.pagingCount = p.count, p.last = p.count - 1), p.computedW = p.itemW - p.boxPadding
            }, p.update = function (e, t) {
                p.doMath(), f || (e < p.currentSlide ? p.currentSlide += 1 : e <= p.currentSlide && 0 !== e && --p.currentSlide, p.animatingTo = p.currentSlide), m.controlNav && !p.manualControls && ("add" === t && !f || p.pagingCount > p.controlNav.length ? c.controlNav.update("add") : ("remove" === t && !f || p.pagingCount < p.controlNav.length) && (f && p.currentSlide > p.last && (--p.currentSlide, --p.animatingTo), c.controlNav.update("remove", p.last))), m.directionNav && c.directionNav.update()
            }, p.addSlide = function (e, t) {
                var n = d(e)
                p.count += 1, p.last = p.count - 1, h && v ? void 0 !== t ? p.slides.eq(p.count - t).after(n) : p.container.prepend(n) : void 0 !== t ? p.slides.eq(t).before(n) : p.container.append(n), p.update(t, "add"), p.slides = d(m.selector + ":not(.clone)", p), p.setup(), m.added(p)
            }, p.removeSlide = function (e) {
                var t = isNaN(e) ? p.slides.index(d(e)) : e;
                --p.count, p.last = p.count - 1, isNaN(e) ? d(e, p.slides).remove() : h && v ? p.slides.eq(p.last).remove() : p.slides.eq(e).remove(), p.doMath(), p.update(t, "remove"), p.slides = d(m.selector + ":not(.clone)", p), p.setup(), m.removed(p)
            }, c.init()
        }, d.flexslider.defaults = {
            namespace: "flex-",
            selector: ".slides > li",
            animation: "fade",
            easing: "swing",
            direction: "horizontal",
            reverse: !1,
            animationLoop: !0,
            smoothHeight: !1,
            startAt: 0,
            slideshow: !0,
            slideshowSpeed: 7e3,
            animationSpeed: 600,
            initDelay: 0,
            randomize: !1,
            pauseOnAction: !0,
            pauseOnHover: !1,
            useCSS: !0,
            touch: !0,
            video: !1,
            controlNav: !0,
            directionNav: !0,
            prevText: "Previous",
            nextText: "Next",
            keyboard: !0,
            multipleKeyboard: !1,
            mousewheel: !1,
            pausePlay: !1,
            pauseText: "Pause",
            playText: "Play",
            controlsContainer: "",
            manualControls: "",
            sync: "",
            asNavFor: "",
            itemWidth: 0,
            itemMargin: 0,
            minItems: 0,
            maxItems: 0,
            move: 0,
            start: function () {},
            before: function () {},
            after: function () {},
            end: function () {},
            added: function () {},
            removed: function () {}
        }, d.fn.flexslider = function (n) {
            if (void 0 === n && (n = {}), "object" == typeof n) return this.each(function () {
                var e = d(this),
                    t = e.find(n.selector ? n.selector : ".slides > li")
                1 === t.length ? (t.fadeIn(400), n.start && n.start(e)) : null == e.data("flexslider") && new d.flexslider(this, n)
            })
            var e = d(this).data("flexslider")
            switch (n) {
                case "play":
                    e.play()
                    break
                case "pause":
                    e.pause()
                    break
                case "next":
                    e.flexAnimate(e.getTarget("next"), !0)
                    break
                case "prev":
                case "previous":
                    e.flexAnimate(e.getTarget("prev"), !0)
                    break
                default:
                    "number" == typeof n && e.flexAnimate(n, !0)
            }
        }, u = Modernizr.touch, p = Modernizr.placeholder, e = $("html").hasClass("lt-ie9"), window.Retina ? Retina.isRetina() ? $("html").addClass("retina") : $("html").addClass("no-retina") : $("html").addClass("no-retina"), $(window).load(r), $(window).resize(r), window.DeviceOrientationEvent && window.addEventListener("orientationchange", r, !1), supernavs_loaded = !1, $().ready(function () {
            var e, t, n, i, a, o, s, r, l, c, d = $("#container")
            d.masonry({
                itemSelector: ".tier-1"
            }), (t = function (e) {
                e.matches ? d.masonry("destroy") : d.masonry()
            })(e = window.matchMedia("all and (max-width: 400px)")), e.addListener(t), $("body#homepage").length && ((n = $("#launch-shell")).toggle(), $.get("https://console.lfe.org/lfe-dot-org-live-consoles-status", function (e) {
                "OK" == e.status && n.toggle()
            })), $("#close-lfe-network").click(function () {
                return $("body, html").animate({
                    scrollTop: $("#lfe-network").offset().top
                }, 300), !1
            }), $("#lfe-network").click(function () {
                return $("body, html").animate({
                    scrollTop: $("#top").offset().top
                }, 300), !1
            }), $("#back-to-top-1, #back-to-top-2").click(function () {
                return $("body").animate({
                    scrollTop: $("#lfe-network").offset().top
                }, 500), !1
            }), $("#start-shell").click(function (e) {
                var t, n, i = "https://console.lfe.org/lfe-dot-org-console/"
                e.preventDefault(), shellDiv = $($(e.target).data("shell-container")), t = 300, shellDiv.animate({
                    height: t
                }), n = $("<iframe>").width("100%").height(t), $(n).attr("src", i), shellDiv.html(n)
            }), i = u ? "slide" : "fade", window.flexslider ? $("html").addClass("no-flexslide") : (a = $("body"), o = $("html"), a.hasClass("home") && (o.addClass("flexslide"), $("#dive-into-lfe").flexslider({
                animation: i,
                direction: "horizontal",
                animationLoop: !0,
                slideshow: !0,
                slideshowSpeed: 8e3,
                animationSpeed: 600,
                randomize: !1,
                smoothHeight: !1,
                pauseOnAction: !0,
                pauseOnHover: !0,
                useCSS: !0,
                controlNav: !0,
                directionNav: !1,
                prevText: "Prev.",
                nextText: "Next",
                touch: u,
                start: function (e) {
                    $(this).fadeIn(), $("body").removeClass("loading")
                }
            })), a.hasClass("psf") && (o.addClass("flexslide"), $("#sponsor-rotation").flexslider({
                animation: "slide",
                direction: "horizontal",
                animationLoop: !0,
                slideshow: !0,
                slideshowSpeed: 8e3,
                animationSpeed: 600,
                randomize: !0,
                smoothHeight: !1,
                pauseOnAction: !0,
                pauseOnHover: !0,
                controlNav: !1,
                directionNav: !1,
                useCSS: !0,
                touch: u
            }))), window.cookie && u && (s = "lfe-FontSize", r = "body", l = $(r).css("font-size"), $.cookie(s) ? (c = $.cookie(s), $(r).css({
                fontSize: c + (-1 != c.indexOf("px") ? "" : "px")
            })) : $.cookie(s, l, {
                expires: 365
            }), $(".text-reset").bind("click", function () {
                return $(r).css("font-size", l), $.cookie(s, l), !1
            }), $(".text-grow").bind("click", function () {
                var e = $(r).css("font-size"),
                    t = parseFloat(e, 10),
                    n = Math.round(1.125 * t)
                return n && ($(r).css("font-size", n), $.cookie(s, n)), !1
            }), $(".text-shrink").bind("click", function () {
                var e = $(r).css("font-size"),
                    t = parseFloat(e, 10),
                    n = Math.round(.89 * t)
                return n && ($(r).css("font-size", n), $.cookie(s, n)), !1
            })), !1 === p && $("[placeholder]").focus(function () {
                var e = $(this)
                e.val() == e.attr("placeholder") && (e.val(""), e.removeClass("placeholder"))
            }).blur(function () {
                var e = $(this)
                "" !== e.val() && e.val() != e.attr("placeholder") || (e.addClass("placeholder"), e.val(e.attr("placeholder")))
            }).blur().parents("form").submit(function () {
                $(this).find("[placeholder]").each(function () {
                    var e = $(this)
                    e.val() == e.attr("placeholder") && e.val("")
                })
            }), $("input:radio").click(function () {
                $("label:has(input:radio:checked)").addClass("active"), $("label:has(input:radio:not(:checked))").removeClass("active")
            }), $("input:checkbox").click(function () {
                $("label:has(input:checkbox:checked)").addClass("active"), $("label:has(input:checkbox:not(:checked))").removeClass("active")
            }), $("input:radio").each(function () {
                $("label:has(input:radio:checked)").addClass("active")
            }), $("input:checkbox").each(function () {
                $("label:has(input:checkbox:checked)").addClass("active")
            }), $.ajax({
                url: "https://2p66nmmycsj3.statuspage.io/api/v2/status.json"
            }).done(function (e) {
                var t = "lfe-status-indicator-" + e.status.indicator
                $("#lfe-status-indicator").removeClass("lfe-status-indicator-default").addClass(t).parent().attr("title", e.status.description)
            })
        })
}).call(this)
