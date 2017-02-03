/* Javascript boilerplate for dynamictable web pages */

/* Adaptation of a nice generic drag-n-drop by @robertc:
   https://stackoverflow.com/a/6239882
   Used to allow filter widgets to be repositioned */

function load() {
    document.body.addEventListener('dragover',drag_over,false);
    document.body.addEventListener('drop',drop,false);
    // Firefox bug where drag events interfer with normal interactions
    // of input fields - <input> events temporarily disable dragging :
    // https://stackoverflow.com/a/21682033
    var inps = document.getElementsByTagName('input');
    for (var i = 0; i < inps.length; i++) {
        var inp = inps[i];
        var par = inp.parentNode;
        if (par) {
            par = par.parentNode;
            if (par.getAttribute('draggable')) {
                inp.addEventListener('focus', _dragOff);
                inp.addEventListener('blur', _dragOn);
            }
        }
    }
}
function _dragOff(){this.parentNode.parentNode.setAttribute('draggable',false)}
function _dragOn() {this.parentNode.parentNode.setAttribute('draggable',true)}
var nowDragging;
function drag_start(event) {
    nowDragging = event.target;
    var style = window.getComputedStyle(event.target, null);
    event.dataTransfer.setData('text/plain',
     (parseInt(style.getPropertyValue('left'),10) - event.clientX) + ',' +
     (parseInt(style.getPropertyValue('top'),10) - event.clientY));
}
function drop(event) {
    var offset = event.dataTransfer.getData('text/plain').split(',');
    nowDragging.style.left = (event.clientX + parseInt(offset[0],10)) + 'px';
    nowDragging.style.top = (event.clientY + parseInt(offset[1],10)) + 'px';
    event.preventDefault();
    return false;
}
function drag_over(event) {
    event.preventDefault();
    return false;
}

/* Basic cross-browser hacks */

function normEvent (e) {
    // Normalize an event object
    if (e) return e;
    return window.event;
}
function stopEvent (e) {
    // Stop event propagation
    // https://stackoverflow.com/a/387823
    if (e.stopPropagation) { e.stopPropagation() } else {e.cancelBubble = true}
}
function normTarg (e) {
    // Normalize the target for an event
    // https://stackoverflow.com/a/1553668
    var targ = e.target || e.srcElement;
    if (targ.nodeType == 3) targ = targ.parentNode; // defeat Safari bug
    return targ;
}
function hasClass (obj, cls) {
    // https://stackoverflow.com/a/5898748
    return (' ' + obj.className + ' ').indexOf(' ' + cls + ' ') > -1;
}

/* DHTML interactivity of various widgets */

function tTog (obj) {
    // Toggle visibility of truncated text in one cell. Based on
    // attribute-link styles.
    var targ = obj.previousSibling;
    var vVal = targ.getAttribute('visible') == 'yes' ? 'no' : 'yes';
    targ.setAttribute('visible',vVal)
    obj.setAttribute('visible',vVal)
}
function truncCol (e, ind) {
    /* Toggle truncated text visibility for an entire
       column. Considered doing this by !important table-level
       attribute styles, but that would not allow individual cells to
       be tweaked (ie, to expand all and then hide some). A CSS
       solution would be more elegant and faster, but would likely
       cause some confusion / irritation. */

    e = normEvent(e);
    var obj  = normTarg(e);
    var vVal = obj.getAttribute('visible') == 'yes' ? 'no' : 'yes';
    var ind  = parseInt( obj.getAttribute('ind') ) - 1;
    var tab  = document.getElementById('data');
    var trs  = tab.getElementsByTagName('tr');
    var tl   = trs.length;
    for (var t = 1; t < tl; t++) {
        var tr = trs[t]
        var cell = tr.childNodes[ind];
        var tt = cell.getElementsByClassName('trunc');
        if (tt.length) {
            // There is a truncatable element in the cell (we assume only one)
            tt[0].setAttribute('visible',vVal);
            // Also set attribute for the '...' widget so it styles correctly
            if (tt[0].nextSibling) tt[0].nextSibling.setAttribute('visible',vVal);
        }
    }
    obj.setAttribute('visible',vVal);
    stopEvent(e);
}
function fTog (e) {
    // Toggle visibilty of column filter interface, using attribute CSS
    e = normEvent(e);
    var targ = normTarg(e).nextSibling;
    if (targ.getAttribute('visible') == 'yes') {
        targ.setAttribute('visible', 'no');
    } else {
        targ.setAttribute('visible', 'yes');
    }
    if (!targ.dragSet) {
        // Set the drag listener
        targ.dragSet = true;
        targ.addEventListener('dragstart',drag_start,false);
    }
    stopEvent(e)
}
function fClose (e) {
    // Close a pop-up widget
    normTarg(normEvent(e)).parentNode.setAttribute('visible', 'no');
    stopEvent(e)
}
function closeAll(e) {
    // Close all popup widgets
    e = normEvent(e);
    var par = normTarg(e).parentNode.parentNode;
    var boxes = par.getElementsByClassName('filtBox');
    for (var i = 0; i < boxes.length; i++) {
        boxes[i].setAttribute('visible', 'no');
    }
    stopEvent(e)
}

/* Functionality for faceting with factor buttons */

function factClick(e) {
    // Toggles the 'picked' attribute when a button is clicked
    e = normEvent(e);
    var obj = normTarg(e);
    // If event was on a child object get parent instead. Issues with FF/chrome
    if (obj.tagName.toLowerCase() == 'span') obj = obj.parentNode;
    var pck = obj.getAttribute('picked');
    obj.setAttribute('picked', pck == 'yes' ? 'no' : 'yes');
    setFactorFilters( obj.getAttribute('what') );
    updateFactorIndicator( obj );
    stopEvent(e)
}
function updateFactorIndicator (obj) {
    var ind = document.getElementById( obj.getAttribute('indicator') );
    if (!ind) return;
    // If the clicked object is an in-line column widget, update indicator:
    // Tally up all current picks and affected rows
    var allButts = obj.parentNode.getElementsByClassName('button');
    var abl      = allButts.length;
    var numButt  = 0, numOff = 0;
    for (var i = 0; i < abl; i++) {
        var nr = parseInt(allButts[i].getAttribute('numrow'));
        if (nr > 0) {
            if (allButts[i].getAttribute('picked') == "yes") {
                // This level is selected, tally the rows it represents
                numButt++;
            } else {
                //
                numOff += nr;
            }
        }
    }
    var filtered = "no";
    if (numButt > 0 && numOff > 0) {
        // We report on number *hidden*, as we can't guarantee that
        // the selected rows will be shown (could be hidden by another
        // column's filter)
        filtered = "yes";
        ind.title = numButt + " level"+(numButt == 1 ? '' : 's')+" shown, which hides "+numOff+" row"+(numOff == 1 ? '' : 's');
    } else if (numButt > 0) {
        // Implies all levels have been selected === no levels selected
        ind.title = "All levels selected, so this filter is not hiding any rows";
    } else {
        ind.title = "No levels selected, click to choose filters";
    }
    ind.setAttribute('filtered', filtered);
}
function allFactorButtons () {
    /* Originally factor filter widgets were actual button elements,
       which made collecting them all relatively easily. Apparently
       buttons are not supposed to have child elements, which was
       upsetting chrome. So they're now divs, so a bit more work
       needed to pick them out. */
    if (!document.AllButtons) {
        // Use a parasitic field to cache the button list
        document.AllButtons = [];
        var divs = document.getElementsByTagName('div');
        dl = divs.length;
        for (var d = 0; d < dl; d++) {
            var div = divs[d];
            if (hasClass(div, 'button')) {
                // This is a filter button
                document.AllButtons.push(div);
                var par = div.parentNode.parentNode;
                if (hasClass(par, 'filtBox')) {
                    // This was an in-line column widget. Note the
                    // widget so we can toggle indicator status
                    div.setAttribute('indicator', par.previousSibling.id)
                }
            }
        }
    }
    return document.AllButtons;
}
function allResetButtons() {
    if (!document.AllResetButtons) {
        // Use a parasitic field to cache the button list
        document.AllResetButtons = [];
        var butts = document.getElementsByTagName('button');
        dl = butts.length;
        for (var d = 0; d < dl; d++) {
            var butt = butts[d];
            if (hasClass(butt, 'reset')) {
                // This is a reset button
                document.AllResetButtons.push(butt);
                var par = butt.parentNode.parentNode;
                if (hasClass(par, 'filtBox')) {
                    // This was an in-line column widget. Note the
                    // widget so we can toggle indicator status
                    butt.setAttribute('indicator', par.previousSibling.id)
                }
            }
        }
    }
    return document.AllResetButtons;
}
function setFactorFilters( request ) {
    // Inspects all the buttons to determine which filters are on
    var bts = allFactorButtons();
    var bl  = bts.length;
    var sets = {};
    for (var b = 0; b < bl; b++) {
        var bt = bts[b];
        var what = bt.getAttribute('what');
        var cls  = bt.className.replace('button ', '');
        var pck  = bt.getAttribute('picked') || 'no';
        if (!sets[ what ]) sets[what] = {};
        if (!sets[ what ][ pck ]) sets[ what ][ pck ] = [];
        sets[ what ][ pck ].push('No'+cls)
    }
    var hide = [];
    var isFilt = {}
    for (var what in sets) {
        var set = sets[ what ];
        if (set.no && set.yes) {
            // Some buttons are checked, others are not
            hide = hide.concat(set.no);
            isFilt[ what ] = 'yes';
        } else {
            // Unfiltered
            isFilt[ what ] = 'no';
        }
    }
    var w = document.getElementById('wrapper');
    w.className = hide.join(' ');
    // The calling function may need to change the 
    return isFilt[ request ];
}

/* Functionality for sorting columns */

function sortFunc(a,b) {
    // Sort by value, or prior row order if the same
    if (a[1] < b[1]) { return -1 } else
    if (a[1] > b[1]) { return  1 } else
    if (a[0] < b[0]) { return -1 } else
    if (a[0] > b[0]) { return  1 } else { return 0 }
}
function doSort(e) {
    // Sorts the rows of the table by a clicked-on column header
    // Some children should not trigger the sort
    e = normEvent(e);
    var obj   = normTarg(e);
    while (1) {
        if (!obj) break;
        var tn = obj.tagName.toLowerCase();
        if (tn == 'th' || tn == 'div') break;
        obj = obj.parentNode;
    }
    // Some child element clicks (filter boxes) should not trigger a sort:
    if (obj.tagName.toLowerCase() != 'th') return;
    var tab   = document.getElementById('data');
    var srt   = obj.getAttribute('sorted');
    // Clear old sort tokens
    var ths   = tab.getElementsByTagName('th');
    for (var t = 0; t < ths.length; t++) {
        ths[t].setAttribute('sorted','');
    }
    // Toggle the sort direction
    if (srt == 'up') {
        srt = -1;
        obj.setAttribute('sorted','down')
    } else {
        srt = 1;
        obj.setAttribute('sorted','up')
    }
    var ind = parseInt( obj.getAttribute('ind') ) - 1;
    var isnum = obj.getAttribute('isnum') || 0;
    var trs   = tab.getElementsByTagName('tr');
    var tl  = trs.length;
    var data = [];
    for (var t = 1; t < tl; t++) {
        var tr = trs[t]
        var val = tr.childNodes[ind].textContent;
        val = isnum ? parseFloat(val) : val.toLowerCase();
        /* trs is not just an array - it's a 'live' object. We can not
           stuff rows back into the table by referencing trs[ index ],
           we need the actual row - hold onto it in data */
        data.push([t,val, tr])
    }
    data = data.sort(sortFunc);
    if (srt < 0) data = data.reverse();
    var dl = data.length;
    for (d=0; d < dl; d++) {
        // Now replace the sorted row objects
        tab.appendChild(data[d][2]);
    }
}

/* Filtering functions - text and numeric */

function textfilt (obj) {
    // Filter text rows based on supplied string. We'll go ahead and
    // allow the text to be treated as a raw regular expression
    var par  = obj.parentNode; // Parent filter UI
    var ind  = parseInt(par.getAttribute('col')); // R column index
    var fNm  = 'filt' + ind; // Filter attribute for this column
    ind--; // R index -> JS index
    
    // Presume most users will not care about case-sensitive matching
    var testVal = obj.value;
    var RE = new RegExp(testVal,"i");
    
    var filtFunc = function (tr) {
        var val  = cellVal(tr.childNodes[ind]);
        return RE.test(val);
    };
    _filterTable(filtFunc, fNm, ind,
                 "do not match '"+testVal +"'");
}
function rangefilt (obj) {
    // Filter rows that fall outside a user-requested numeic range
    var par  = obj.parentNode; // Parent filter UI
    var ind  = parseInt(par.getAttribute('col')); // R column index
    var fNm  = 'filt' + ind; // Filter attribute for this column
    ind--; // R index -> JS index

    // Get the minimum and maximum values to show
    var rngs = par.getElementsByTagName('input');
    var min  = parseFloat(rngs[0].value)
    if (isNaN(min)) min = -Infinity;
    var max  = parseFloat(rngs[1].value)
    if (isNaN(max)) max = Infinity;

    var filtFunc = function (tr) {
        var val  = parseFloat(cellVal(tr.childNodes[ind]));
        return !isNaN(val) && val >= min && val <= max
    };
    // Crude signif truncation of unseemly high-precision numbers
    minTxt = (min+"").substr(0,5).replace(/\.$/,'');
    maxTxt = (max+"").substr(0,5).replace(/\.$/,'');
    _filterTable(filtFunc, fNm, ind,
                 'hidden where x<'+minTxt+' or x>'+maxTxt);
}
function intfilt (obj) {
    // Range filter request that occurs when the color scale is clicked on
    var par  = obj.parentNode; // Parent filter UI
    var inps = par.getElementsByTagName('input'); // The min/max fields
    var min  = obj.getAttribute('min');
    if (min == null) min = inps[0].getAttribute('reset');
    inps[0].value = min;
    var max  = obj.getAttribute('max');
    if (max == null) max = inps[1].getAttribute('reset');
    inps[1].value = max;
    return rangefilt( inps[0] )
}

function _filterTable (filtFunc, fNm, ind, msg) {
    // Apply a calback to each row to decide if it should be filtered or not
    var tab  = document.getElementById('data');
    var trs  = tab.getElementsByTagName('tr');
    var tl   = trs.length;
    var numHidden = 0, numShown = 0;
    for (var t = 1; t < tl; t++) {
        var tr = trs[t];
        if (filtFunc(tr)) {
            // A true value indicates the row should be shown (filter
            // attribute set to 'no')
            tr.setAttribute(fNm, 'no');
            numShown++;
        } else {
            tr.setAttribute(fNm, 'yes');
            numHidden++;
        }
    }

    // Update the filter icon to indicate if any filters are active
    var th   = trs[0].getElementsByTagName('th')[ind];
    var icon = th.getElementsByClassName('fTog')[0];
    if (icon) {
        var ttxt = 'No rows hidden by this filter, click to change';
        if (numHidden == 0) {
            icon.setAttribute('filtered', 'no');
        } else {
            icon.setAttribute('filtered', 'yes');
            ttxt = numHidden + ' row' + (numHidden == 1 ? '' : 's')+
                (numShown == 0 ? ' [THE WHOLE TABLE!!]' : '')+' '+msg;
        }
        icon.setAttribute('title', ttxt);
    }
    return numHidden;
}

function cellVal (td) {
    var rv = td.getAttribute('txt');     // Stored, unaltered value
    if (rv == null) rv = td.textContent; // Otherwise, use cell text
    return rv;
}
function resetAll(e) {
    // Resets all filters
    var bts = allResetButtons();
    var bl  = bts.length;
    for (var b = 0; b < bl; b++) {
        var bt = bts[b];
        resetFilt( bt, true);
    }
    setFactorFilters( '' ); // Process all factor filters in bulk

    e = normEvent(e);
    stopEvent(e)
}
function submitFilt (obj) {
    // Filters will trigger with onchange, but also here if a submit
    // button is clicked
    var par  = obj.parentNode;
    var ft   = par.getAttribute('ftype') || ''; // The type of filter
    var fObj = obj.previousSibling.previousSibling;
    if (ft == 'num') {
        return rangefilt(fObj); // Numeric
    } else {
        return textfilt(fObj);  // Text
    }
    // Factors do not have submit buttons, they use per-level toggles
}
function resetFilt(obj, noAction) {
    // Restor a numeric or text filter to unfiltered (full range)
    var par  = obj.parentNode;
    var ft   = par.getAttribute('ftype') || ''; // The type of filter
    if (ft == 'num') {
        var inps = par.getElementsByTagName('input');
        for (var i = 0; i < inps.length; i++ ) {
            var inp = inps[i];
            inp.value = inp.getAttribute('reset');
        }
        return rangefilt( inps[0] );
    } else if (ft == 'fact') {
        // Get each factor button and set 'picked' to 'no'
        var butts = par.getElementsByTagName('div');
        var bl = butts.length;
        for (var i = 0; i < bl; i++) {
            var picked = butts[i].getAttribute('picked');
            if (picked == 'yes') butts[i].setAttribute('picked', 'no');
        }
        updateFactorIndicator( butts[0] );
        if (!noAction) return setFactorFilters("");
    } else {
        var inps = par.getElementsByTagName('input');
        inps[0].value = '';
        return textfilt( inps[0] );
    }
    return false;
}
