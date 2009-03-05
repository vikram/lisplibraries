function UCWInputCompleter (id_prefix) {

    if (id_prefix == null) {
	alert('Must specify id_prefix parameter in UCWInputCompleter');
    }

    //////////////////////////////
    //// Public API 
    // The maximum number of results we show. null means all elements
    // will always be shown
    this.num_results = null;
    // The minimum number of chartces in input before completion kigks in
    this.min_input_chars = 1;
    // if true then we fillout the value when only one completion is
    // found
    this.auto_complete = true;
    // The id of the input tag element we're attached to
    this.input_id = id_prefix + '-input';
    // The id of the result div we use to show the completions
    this.result_id = id_prefix + '-results';
    // The id used to tag the highlighted result
    this.highlight_id = id_prefix + '-highlight';
    // The array of possible completions
    this.table = new Array();
    // The function used to test whether a value could be acompletion
    // of the current input
    this.is_completion_of = function (completion, input) {
	return completion.toLowerCase().indexOf(input.toLowerCase()) == 0;
    }
    // The function used to sort two valid completions
    this.sort = function (a, b) { 
	if (a < b) {
	    return -1;
	}
	if (a > b) {
	    return 1;
	}
	return 0;
    }
    // call this function via the body tag's onLoad method.
    this.load = function () {
	var input = document.getElementById(this.input_id);
	var result = document.getElementById(this.result_id);
	if (input == null) {
	    alert('Unable to find the element with id ' + this.input_id);
	    return;
	} 
	if (result == null) {
	    alert('Unable to find the element with id ' + this.result_id);
	    return;
	}
	// we need to "capture" the current value of this
	var self = this;
	input.onkeypress = function (event) { self.handle_key_press(event || window.event) };
	input.onkeyup = function (event) { self.handle_key_up(event || window.event) };
	input.onblur = function (event) { self.clear_results(event || window.event) };
	input.setAttribute("autocomplete", "off");
	result.style.display = 'none';
    }

    //////////////////////////////
    //// Implementation

    this.first_result_element = null;
    this.last_result_element = null;
    
    this.complete = function (input) {
	var results = new Array();
	for (var i = 0; i < this.table.length; i++) {
	    var current = this.table[i];
	    if (this.is_completion_of(current, input)) {
		results.push(current);
	    }
	}
	results = results.sort(this.sort);
	return this.num_results == null ? results : results.splice(0, this.num_results);
    }
	
    this.highlighted_element = function () {
	return document.getElementById(this.highlight_id);
    }	

    this.do_completion = function (event) {
	var input = document.getElementById(this.input_id);
	var value = input.value;
	if (value.length >= this.min_input_chars) {
	    var results = (this.complete(value) || new Array()).sort(this.sort);
	    if (results.length == 1 && this.auto_complete) {
		if (key_code(event.keyCode) == 8) {
		    // just render after a backspace
		    this.render_results(results);
		} else { 
		    input.value = results[0];
		    this.clear_results();			
		}
	    } else {
		this.render_results(results);
	    }
	} else {
	    this.clear_results();
	}
    }

    this.make_result_li = function (result) {
	// given result (generally a string of text) create a new LI
	// tag which will be insterted in the html and shown to the
	// user.
	var li = document.createElement("li");
	li.setAttribute("ucw-result-text", result);
	var a = document.createElement("a");
	li.appendChild(a);
	a.appendChild(document.createTextNode(result));
	return li;
    }

    this.clear_results = function () {
	var result_div = document.getElementById(this.result_id);
	var child = result_div.firstChild;
	while (child != null) {
	    result_div.removeChild(child);
	    child = result_div.firstChild;
	}
	result_div.style.display = 'none';
	if (this.highlighted_element()) {
	    this.highlighted_element().removeAttribute("id");
	}
    }

    this.render_results = function (results) {
	this.clear_results();

	var result_div = document.getElementById(this.result_id);
	var ul = document.createElement("ul")
	result_div.appendChild(ul);
	for (var i = 0; i < results.length; i++) {
	    ul.appendChild(this.make_result_li(results[i]));
	}
	this.first_result_element = ul.firstChild;
	this.last_result_element = ul.lastChild;
	result_div.style.display = 'block';
    }

    function key_code (keyCode) {
	switch (keyCode) { // for Safari
	case 63276: return 33; break;
	case 63277: return 34; break;
	case 63275: return 35; break;
	case 63273: return 36; break;
	case 63234: return 37; break;  
	case 63232: return 38; break;
	case 63235: return 39; break;  
	case 63233: return 40; break;  
	default: return keyCode;
	}
    }

    this.handle_key_press = function (event) {
	var code = key_code(event.keyCode);
	if (code == 40) { // DOWN
	    this.highlight_next();
	    this.preventDefault(event);
	} else if (code == 38) { // UP
	    this.highlight_previous();
	    this.preventDefault(event);
	} else if (code == 27) { // ESCAPE
	    this.clear_results();
	    this.preventDefault(event);
	} else if (code == 13) { // ENTER
	    var element = this.highlighted_element();
	    if (element) {
		var input = document.getElementById(this.input_id);
		input.value = element.getAttribute("ucw-result-text");
		this.preventDefault(event);
		this.clear_results();
	    }
	    this.preventDefault(event);
	}
    };

    this.handle_key_up = function (event) {
	var code = key_code(event.keyCode);
	if (code != 40 && code != 38 &&
	    code != 27 && code != 13) {
	    this.do_completion(event);
	} else if (!event.preventDefault) { // IE
	  this.handle_key_press (event);
	}
    };

    this.set_highlighted = function (element) {
	// set element as the new highlighted result. un highlight the
	// previously highlighted element (if there was one).
	if (element) {
	    if (this.highlighted_element()) {
		this.highlighted_element().removeAttribute("id");
	    }
	    element.setAttribute("id", this.highlight_id);
	}
    }

    this.highlight_next = function () {
	if (this.highlighted_element()) {
	    this.set_highlighted(this.highlighted_element().nextSibling || this.first_result_element);
	} else {
	    this.set_highlighted(this.first_result_element);
	}
    }

    this.highlight_previous = function () {
	if (this.highlighted_element()) {
	    this.set_highlighted(this.highlighted_element().previousSibling || this.last_result_element);
	} else {
	    this.set_highlighted(this.last_result_element);
	}
    }
    this.preventDefault = function (event) {
      if (event.preventDefault) // FF
        event.preventDefault();
      else // IE
        event.returnValue = false;
    }    
}
