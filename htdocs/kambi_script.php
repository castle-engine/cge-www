<?php
  require_once 'vrmlengine_functions.php';
  common_header('KambiScript language', LANG_EN);

  function func_ref($name, $title)
  {
    echo '<a href="#function_' . $name . '"><tt>' . $title . '</tt></a>';
  }

  function func($name, $title)
  {
    echo '<a name="function_' . $name . '" class="kscript_func_docs"><tt>' . $title . '</tt></a>';
  }
?>

<?php echo pretty_heading('KambiScript language', NULL,
'Simple scripting language for Kambi VRML game engine'); ?>

<p><tt>KambiScript</tt> is a simple scripting language used in
our <i>Kambi VRML game engine</i>. You can use it in VRML/X3D <tt>Script</tt>
nodes. Also it's syntax of mathematical expressions is usable
throughout our engine, for example <?php echo a_href_page(
'glplotter and gen_function',
'glplotter_and_gen_function'); ?> (which are completely
not related to VRML) use this syntax to define function expressions.</p>

<p>The language is deliberately very simple. It's a scripting language,
with features inspired by many other languages, and by author's laziness.
For example I was too lazy to add <tt>if</tt>, <tt>while</tt> and such
constructs to the grammar,
instead you have built-in functions like
<?php func_ref('if', 'if(condition, then_code, else_code)'); ?>.
<b>This language doesn't try to compete with other scripting languages</b>
(like <i>ECMAScript</i>, commonly used in VRML scripting).
It's not suitable for larger programs
(for starters, you cannot define your own types).<!-- ; you also cannot
currently call user-defined functions (you can only call built-in functions;
user-defined functions are only automatically called when VRML Script
receives an event); also you cannot declare your own local variables
(but you can use VRML Script initializeOnly fields for this purpose)-->
Also it's specific to our engine, and probably always will be.</p>

<p>That said, the language is powerful enough for many uses.
You can process all VRML field types with it, including strings,
vectors, matrices and even images. Also arrays (VRML MFXxx fields) are covered.
There are many built-in functions and operators, heavily overloaded
for all types where they apply (for example, you can add numbers,
vectors, matrices or strings).
It's an integral part of our engine, without the need for any external libraries.
And do note that our engine doesn't support (yet) ECMAScript for VRML script
at all, so this is the only way to do scripting for now (without
writing and compiling any ObjectPascal code).</p>

<p>Programmers may also be interested that language implementation is flexible,
you can extend it easily from ObjectPascal (adding new data-types and
built-in functions), for many uses (not necessarily related with VRML).
The language is completely safe (that is, there's no possibility for
malicious script author to do something harmful)
simply because the language is a closed data-processing language
(the only I/O routines,
<?php func_ref('image_load', 'image_load(url)'); ?> and
<?php func_ref('writeln', 'writeln(string)'); ?>,
expose functionality that is possible anyway with
pure non-scripted VRML).</p>

<?php
  $toc = new TableOfContents(
    array(
      new TocItem('Writing scripts inside VRML Script URLs', 'script_urls'),
      new TocItem('Examples', 'examples'),
      new TocItem('Syntax', 'syntax'),
      new TocItem('Types and constants', 'types_constants', 1),
      new TocItem('Programs and expressions', 'programs_expressions', 1),
      new TocItem('Built-in functions', 'built_in_functions'),
      new TocItem('Type conversion', 'functions_conversion', 1),
      new TocItem('Control flow', 'functions_control_flow', 1),
      new TocItem('Number (integer and float) functions', 'functions_number', 1),
      new TocItem('Random numbers functions', 'functions_random', 2),
      new TocItem('Boolean functions', 'functions_boolean', 1),
      new TocItem('String functions', 'functions_string', 1),
      new TocItem('Array functions', 'functions_array', 1),
      new TocItem('Vector functions', 'functions_vector', 1),
      new TocItem('Matrix functions', 'functions_matrix', 1),
      new TocItem('Image functions', 'functions_image', 1),
      new TocItem('VRML node functions', 'functions_node', 1),
      new TocItem('Precise grammar', 'precise_grammar')
    )
  );
  $toc->echo_numbers = true;
?>

<?php echo $toc->html_toc(); ?>

<?php echo $toc->html_section(); ?>

<p>URLs in <tt>Script</tt> node starting with <tt>kambiscript:</tt>
are understood to contain program in KambiScript language.
URLs to external files with extension <tt>.kscript</tt> point
to whole files in KambiScript language. Like</p>

<pre class="light_bg">
Script {
  inputOnly SFFloat foo
  outputOnly SFFloat foo_plus_one

  url "kambiscript:
function foo(value, timestamp)
foo_plus_one := value + 1
"
}

Script {
  url "my_script.kscript"
}
</pre>

<?php echo $toc->html_section(); ?>

<p>Some examples of simple mathematical expressions for glplotter:</p>

<pre class="light_bg">
  sin(x) ^ 10
  2 * (cos(ln(x)) - 1)
  sin(x) &gt; cos(x)
  or( sin(x) &gt; cos(x), sin(x) &gt; 0 )
</pre>

<p>Some example of simple program for VRML script node:</p>

<pre class="light_bg">
Script {
  # Let's assume some TouchSensor.touchTime is routed here.
  # When user clicks on this touch sensor, you want to close the door
  # if they are open, or open them if they are closed.
  inputOnly SFTime touch_time

  initializeOnly SFBool open FALSE

  # Let's assume this is routed to some TimeSensor.set_startTime
  # that starts closing animation.
  outputOnly SFTime close_time

  # Let's assume this is routed to some TimeSensor.set_startTime
  # that starts opening animation.
  outputOnly SFTime open_time

  url "kambiscript:
function touch_time(value, timestamp)
if (open,
    close_time := timestamp,
    open_time := timestamp);
open := not(open)
"
}
</pre>

<p>Example script behavior above could also be done by combining
<tt>BooleanToggle</tt>, <tt>BooleanFilter</tt>, <tt>TimeTrigger</tt>
X3D nodes.
But script is already simpler and shorter, and allows you to trivially
add other interesting things.</p>

<pre class="light_bg">
# Simple converter from SFString to MFString using built-in <?php func_ref('array', 'array'); ?> function.
Script {
  inputOnly SFString input
  outputOnly MFString output
    url "kambiscript:
function input(value, timestamp)
  output := array(value)
"
}
</pre>

<p>Some larger examples:
<ul>
  <li><p><a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_test_suite/x3d/kambi_extensions/kambi_script_ball_game.x3dv">kambi_script_ball_game.x3dv</a>
    &mdash; a small X3D game, with whole game logic implemented in KambiScript
    (key handling by KeySensor node). Can be played in any VRML browser
    supporting KambiScript, like <?php echo a_href_page('view3dscene','view3dscene'); ?>
    or any of the example VRML browser components in engine sources.

  <li><p><a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_test_suite/x3d/kambi_extensions/kambi_script_edit_texture.x3dv">kambi_script_edit_texture.x3dv</a>
    &mdash; a toy image editor. Again, it's a pure X3D file (you can
    open it and use with any VRML browser supporting KambiScript).
    Uses KambiScript to implement various simple image editing
    functions. It's a toy, not to be treated as a serious image editor
    of course (there is no possibility to save created image for starter,
    since KambiScript doesn't allow to save files from VRML for safety reasons.)
    But it shows that even image processing is quite easy with KambiScript.

  <li><p><a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_test_suite/x3d/kambi_extensions/kambi_script_particles.x3dv">kambi_script_particles.x3dv</a>
    &mdash; a simple particle engine. Whole particles animation,
    logic (randomization, speed, gravity) is implemented in KambiScript.
    "Particles" are rendered as points and lines (<tt>PointSet</tt>,
    <tt>IndexedLineSet</tt>).
</ul>

<?php echo $toc->html_section(); ?>

<p><i>Syntax is free-form</i>, the only use of whitespace (including newlines,
or any indentation) is to separate tokens when needed (for example, between
two identifiers).</p>

<p><i>Comments</i> are within curly braces: <tt>{ this is a comment }</tt>
(Pascal-like).</p>

<?php echo $toc->html_section(); ?>

<p><i>Types</i> are never explicitly declared, and are checked
at runtime. Four core types are available:</p>

<ol>
  <li><p><i>Integers.</i>
    Syntax of integer constants is obvious,
    like <tt>123</tt>. Built-in function
    <?php func_ref('int', 'int(...)'); ?> allows
    you to convert other core types into integer.</p>

    <p>We use 64-bit signed integers (although for VRML long/int32
    fields, they will have to fit within 32-bit.)</p>

    <p>Specifically for comfortable processing of
    <a href="http://web3d.org/x3d/specifications/ISO-IEC-FDIS-19775-1.2-X3D-AbstractSpecification/Part01/components/keyboard.html#KeySensor">X3D
    KeySensor node</a> events <tt>actionKeyPress/Release</tt>
    you have 20 key constants available: <tt>ACTION_KEY_F1</tt>,
    ... <tt>ACTION_KEY_F12</tt>, <tt>ACTION_KEY_HOME</tt>, etc.
    (see KeySensor specification for full list).</p></li>

  <li><p><i>Floats.</i> Syntax of float constants
    is also obvious, like <tt>3.1415</tt>. You have
    constants <tt>pi</tt> and <tt>enat</tt> (Euler's number).
    Built-in function
    <?php func_ref('float', 'float(...)'); ?> allows
    you to convert other core types into float.</p>

    <p>Precision: uses the best floating-point type precision on given
    platform, which means at least Double, and on many platforms
    Extended.</p></li>

  <li><p><i>Booleans.</i> Two obvious constants are available, <tt>false</tt>
    and <tt>true</tt> (case is ignored, as usual in KambiScript,
    so you can also write uppercase
    <tt>FALSE</tt> or <tt>TRUE</tt> like in classic VRML).
    Built-in function
    <?php func_ref('bool', 'bool(...)'); ?> allows
    you to convert other core types into boolean.</p></li>

  <li><p><i>Strings.</i> Syntax of constants is Pascalish (in apostrophes, and two
    consecutive apostrophes inside mean that you want a single literal
    apostrophe character). For example <tt>'He said "It''s mine."'</tt>.
    Apostrophe was chosen not only because, y'know, it's Pascalish :),
    but also because it makes embedding KambiScript code within
    VRML string easier (no need to escape quotes by backslashes).
    You can make actual newlines within the string, like in VRML.
    For example:
<pre class="light_bg">
Script {
  # Let's assume some TouchSensor.touchTime is routed here.
  inputOnly SFTime touch_time
  outputOnly MFString text

  url "kambiscript:
function touch_time(value, timestamp)
  text := array(
    'First string of text clicked on ' + string(value),
    'Second string of text.
Still second string of text.
As you see, you can simply make a newline in the string literal to get a newline inside the string.'
  )
"
}
</pre>

    <p>Built-in function
    <?php func_ref('string', 'string(...)'); ?> allows
    you to convert other core types into string.</p></li>
</ol>

<p>The one and only implicit type conversion (promotion) of types is from
integer to float (for example, <tt>my_float := 44</tt> works,
you don't have to write <tt>my_float := 44.0</tt>).
In particular, note that <i>boolean type is not interchangeable
with integer</i> like in C. If you want to convert between boolean and integer,
you have to convert explicitly by <tt>bool(my_int)</tt> or <tt>int(my_bool)</tt>,
like in Pascal. The only exception is when using KambiScript solely for
mathematical expressions (like in <?php echo a_href_page(
'glplotter and gen_function',
'glplotter_and_gen_function'); ?>, internally using <tt>ParseFloatExpression</tt>
function): in this case, result is always implicitly converted to float,
like it would be embedded within <tt>float(...)</tt> call.
</p>

<p>When using KambiScript inside VRML scripts, internally you have
all the VRML field types available (which means that
vec2f, vec3f, vec4f, matrix, image and others are included).
There is no special syntax for reading/writing other types, instead
you have many functions to construct and set them.
For example for vec3f type you have "constructor"
<?php func_ref('vector', 'vector(x, y, z)'); ?> ,
reader for particular component <?php func_ref('vector_get', 'vector_get(vector, index)'); ?>,
and setter for particular component <?php func_ref('vector_set', 'vector_set(vector, index, component_value)'); ?>.
Even images have functions to create and modify them, which means
that you can use KambiScript to perform basic image creation and processing.</p>

<p>Also array types are internally available, for VRML multiple-value
(MFXxx) types. Again no special syntax is available (sorry, no bracket parenthesis),
but there are functions to construct array
<?php func_ref('array', 'array(item1, item2, ...)'); ?>,
read component <?php func_ref('array_get', 'array_get(array, index)'); ?> and
set component <?php func_ref('array_set', 'array_set(array, index, component_value)'); ?>.

<?php echo $toc->html_section(); ?>

<p><i>Program</i> is just a set of functions. VRML engine will take care
to call function <tt>xxx</tt> when input event of the same name will arrive.</p>

<p><i>Expressions and instructions</i> are the same thing within
the language. For example, "assignment" is an instruction, since
it causes calculation of the right operand and assigning it to the left
operand, but it's also an "expression", since it returns the value
(the assigned value).
So "chained" assignment, like in C, is possible (although
usually discouraged, to not obfuscate the code): <tt>a := b := x + y</tt> works.
In the rest of this description, terms "instruction" and "expression"
mean exactly the same thing.</p>

<p><i>Function</i> starts from the <tt>function</tt> keyword,
then function name (identifier),
then list of 0 or more parameters (identifiers separated by commas),
always within parenthesis. For functions within VRML Script nodes:
<tt>initialize</tt> and <tt>shutdown</tt> must take exactly
one parameter (timestamp of the event: SFTime), functions called
by incoming events must take exactly two parameters (value send to the event,
and timestamp: SFTime).</p>

<p>Function body is just a sequence of expressions separated by
semicolon. Formally, function body is actually a single expression,
but we have a semicolon operator: <tt>A;B</tt> means "calculate A,
ignore result, then calculate and return result of B".
For now, result of functions body is ignored (so all our functions
are in fact <i>procedures</i>).
Semicolon works like a delimiter (not a terminator,
so it's used only between instructions).</p>

<p>Note that the way semicolon and expressions are defined means
that we don't need any special syntax for compound instruction
(like <tt>begin end</tt> in Pascal or
<tt>{ }</tt> in C). Instead, normal parenthesis may be
used if necessary to group instructions.</p>

<p>An <i>assignment instruction</i> is an operand, followed by
the assignment operator <tt>:=</tt> (Pascal-like),
followed by an expression to calculate value to assign.

<p>For VRML scripts, you are allowed to assign to output events
and to fields (exposed or not). Events sending behavior follows ECMAScript
standard:</p>

<ul>
  <li><p>Assigning value to initializeOnly (not exposed) field is simple, it just
    assigns value to this field. You can use initializeOnly fields as
    "variables" available for your scripts (since KambiScript doesn't
    allow you to declare or use new variables within the program, for now).
    </p></li>

  <li><p>Assigning value to output event results in sending this event,
    assigning to exposed field results in sending this to input event of this field.

    <p>Following ECMAScript standard, events are not send immediately
    (right at the assignment), instead they are stacked and send
    when script function finished execution. When you assigned
    multiple values for the same field/event, only the last one is send.
    In case of multiple-value fields, the combined end value is send.
    For example, assuming <tt>output</tt> is an <tt>outputOnly</tt>
    event of MFFloat type:

<pre class="light_bg">
function foo(value, timestamp)
  output := array(0.0, 1.0, 2.0, 3.0);
  array_set(output, 1, 666.0);
  array_set(output, 2, 44.0)
</pre>

    <p>The example above will send one <tt>output</tt> event with value
    <tt>(0.0, 666.0, 44.0, 3.0)</tt>.</p></li>
</ul>

<p>Right side of the assignment instruction is the value to calculate
and assign. In short, a normal mathematical expression is allowed there,
just like you seen in all programming languages. We have multiplicative
operators (<tt>/, *, ^, %</tt>),
we have additive operators (<tt>+, -</tt>) with lower
priority, we have comparison operators
(<tt>&lt;, &gt;, &lt;=, &gt;=, = or &lt;&gt;</tt>) with even lower
priority. We have all standard math
functions. Built-in functions and operators are overloaded
for all suitable types. Section below gives a full list of operators
and functions.</p>

<?php echo $toc->html_section(); ?>

<?php echo $toc->html_section(); ?>

<ul>
  <li><p><?php func('int', 'int(...)'); ?> converts a "core" type
    to an integer.</p>

    <p>Float is converted to int by discarding it's fractional
    part (like in C; for positive numbers, this is like <tt>floor</tt>, for negative
    this is like <tt>ceil</tt>).
    There are also functions <?php func('floor', 'floor'); ?>, <?php func('ceil', 'ceil'); ?> and
    <?php func('round', 'round'); ?> that convert float to an integer with other rounding
    modes.</p>

    <p>Bool is converted to 0 (false) or 1 (true).
    Yes, unlike most languages that usually
    don't guarantee "true" value (saying "true" is anything &lt;&gt; 0),
    KambiScript actually guarantees that "true" will result in 1.
    This is sometimes useful in smart mathematical expressions
    (like <tt>my_int := 1 - int(my_bool)</tt>).</p>

    <p>String is converted to int by, well,
    converting string to integer using standard decimal notation
    (<tt>int('123') = 123</tt>).</p></li>

  <li><p><?php func('float', 'float(...)'); ?> converts a "core" type
    to a float.</p>

    <p>Integer is converted obviously. Actually it's never needed to
    explicitly cast integer to float, this conversion happens automatically,
    like in most programming languages.</p>

    <p>Bool is converted to 0.0 (false) or 1.0 (true).</p>

    <p>String is converted to float by parsing number from string,
    like <tt>float('3.14') = 3.14</tt>.</p></li>

  <li><p><?php func('bool', 'bool(...)'); ?> converts a "core" type
    to a boolean.</p>

    <p>Integers and floats are converted to "false" if equal zero, "true"
    otherwise.</p>

    <p>Strings are converted to booleans recognizing 'false' and 'true'
    strings (and making errors in other cases).</p></li>

  <li><p><?php func('string', 'string(...)'); ?> converts a "core" type
    to a string.</p>

    <p>Not much to write here, numbers (integers and floats) are converted
    to normal notation and boolean is converted to 'false' or 'true'.</p></li>
</ul>

<p>All four basic conversion functions accept also variables that already
have the necessary type. For example, converting float to float is a valid
(and harmless) operation.</p>

<?php echo $toc->html_section(); ?>

<p><?php func('if', 'if(condition, then_code, else_code)'); ?> is our
conditional instruction. <tt>condition</tt> is first calculated, must be a boolean
value. If it's true, then <tt>then_code</tt> is executed and returned as
"if" value. Otherwise, <tt>else_code</tt> is executed and returned as
"if" value. You may note that (because of KambiScript unification of
"instruction" and "expression" terms) this can be used in both
functional and imperative form. That is, all below are valid:</p>

<pre class="light_bg">
  { imperative form }
  if(x &gt; 3, y := 'yes', y := 'no');

  { functional form, equivalent to above, looks more elegant in this case }
  y := if(x &gt; 3, 'yes', 'no');

  { actually, even this is possible if you need it: }
  y_copy := if(x &gt; 3, y := 'yes', y:= 'no');
</pre>

<p><?php func('when', 'when(condition, then_code)'); ?> is
a conditional instruction without the "else" clause.
It's equivalent to <tt>if(condition, then_code, false)</tt>, so it simply
returns <tt>false</tt> when condition is not satisfied.
(This is considered a good thing that the normal <tt>if</tt>
<i>requires</i> the else clause; this way we avoid trivial errors
when programmer forgets to write <tt>else</tt> clause; similar
<tt>when</tt> expression may be found e.g. in Lisp and <a href="http://nemerle.org/">Nemerle</a>.)
</p>

<p><?php func('while', 'while(condition, loop_code)'); ?> performs
a while loop. Calculate <tt>condition</tt> (must yield a boolean value),
if true then execute <tt>loop_code</tt> and again calculate <tt>condition</tt>,
if it's still true then execute <tt>loop_code</tt> again, ... you get the idea.</p>

<p><?php func('for', 'for(counter, begin_value, end_value, loop_code)'); ?> performs
a for loop. <tt>counter</tt> must be an assignable integer variable
(note that for now you cannot declare new variables for KambiScript;
you usually need to overuse <tt>initializeOnly</tt> field of VRML script
node for this). <tt>begin_value</tt>, <tt>end_value</tt> must also
be integer values, will be calculated once at the beginning of the loop.
We will assign to <tt>counter</tt> variable integer values
from <tt>begin_value</tt> to <tt>end_value</tt>, and for each
counter value we will execute <tt>loop_code</tt>.
It's undefined what happens when <tt>loop_code</tt> changes directly the
<tt>counter</tt> value.</p>

<p><tt>for</tt> and <tt>while</tt> loops return
the value of last executed <tt>loop_code</tt>,
or <tt>false</tt> if <tt>loop_code</tt> did not get executed even once.</p>

<?php echo $toc->html_section(); ?>

<p>Self-explanatory math functions are listed below.
They all take a float type, and return a float type unless otherwise noted:</p>

<ul>
  <li><?php func('Sin', 'Sin'); ?>,
      <?php func('Cos', 'Cos'); ?>,
      <?php func('Tan', 'Tan'); ?>,
      <?php func('CoTan', 'CoTan'); ?>
  <li><?php func('ArcSin', 'ArcSin'); ?>,
      <?php func('ArcCos', 'ArcCos'); ?>,
      <?php func('ArcTan', 'ArcTan'); ?>,
      <?php func('ArcCoTan', 'ArcCoTan'); ?>
  <li><?php func('SinH', 'SinH'); ?>,
      <?php func('CosH', 'CosH'); ?>,
      <?php func('TanH', 'TanH'); ?>,
      <?php func('CoTanH', 'CoTanH'); ?>
  <li><?php func('Log2', 'Log2'); ?> (same as <tt>Log(2, x)</tt>),
      <?php func('Ln', 'Ln'); ?>,
      <?php func('Log', 'Log'); ?>,
      <?php func('Power2', 'Power2'); ?> (same as <tt>Power(2, x) = 2^x</tt>),
      <?php func('Exp', 'Exp'); ?> (same as <tt>Power(enat, x) = enat^x</tt>),
      <?php func('Power', 'Power'); ?>,
      <?php func('Sqr', 'Sqr'); ?>,
      <?php func('Sqrt', 'Sqrt'); ?><br>
  <li><?php func('Sgn', 'Sgn'); ?> (returns integer), <?php func('Abs', 'Abs'); ?>
  <li><?php func('Max', 'Max'); ?>, <?php func('Min', 'Min'); ?>
    (any number of arguments &gt;= 1 allowed; works on either floats or ints)
</ul>

<?php echo $toc->html_section(); ?>

<p><?php func('random', 'random()'); ?> returns a random float number
within 0...1 range (0 included, 1 excluded).</p>

<p><?php func('random', 'random(int)'); ?> returns a random integer number
strictly less than <tt>int</tt> and &gt;= 0.
(<tt>int</tt> argument must be &gt; 0).</p>

<?php echo $toc->html_section(); ?>

<p>Basic boolean operations:
<?php func('or', 'or(bool1, bool2...)'); ?>,
<?php func('and', 'and(bool1, bool2...)'); ?> (any number of arguments
&gt;= 1), <?php func('not', 'not(bool1)'); ?>.</p>

<?php echo $toc->html_section(); ?>

<p>You can add (concatenate) and compare (case-sensitive) strings
by normal operators. Converting other
core types (numbers, booleans) to string may be done by the
<?php func_ref('string', 'string(...)'); ?> function.

<p><?php func('writeln', 'writeln(my_string)'); ?> outputs a string.
This is printed on the program's standard error output, or some other
kind of console. (From code, it's configurable where this is printed,
see <tt>Program.Environment.OutputProc</tt>. By default, it results
in <tt>DataNonFatalError</tt>, that by default is simply ignored.
Although my programs usually take care to do something more useful with it.)
This should be used purely for debugging purposes.</p>

<p>Most array functions can also treat the string as an array of characters.
We do not have a special type for a "character" &mdash; we just use a string with length 1.
You can get / set the length of the string with
<?php func_ref('array_get_count', 'array_get_count(string)'); ?> /
<?php func_ref('array_set_count', 'array_set_count(string, count)'); ?>.
And you can get / set a specific character of the string with
<?php func_ref('array_get_count', 'array_get(string, index)'); ?> /
<?php func_ref('array_set_count', 'array_set(string, index, character)'); ?>.
Indexes for characters inside string are zero-based, like for all arrays
in KambiScript.</p>

<p><?php func('character_from_code', 'character_from_code(int)'); ?> converts integer
character code to a 1-letter string with this character.
<i>Only the ASCII character codes are
guaranteed to work in the long run.</i> In the future, all Unicode character codes
will be accepted here, and rendered if present in the font.
Currently, our font rendering is limited to 256-character encodings.</p>

<p>A lot of string functions are trivial to add
&mdash; report if you need some particular function.

<?php echo $toc->html_section(); ?>

<p><?php func('array', 'array(item1, item2, ...)'); ?>
 constructs an array. At least one argument is required.
All arguments must have the same type (VRML multiple-value fields
can't have mixed types).</p>

<p>Note that parameter-less <tt>array()</tt> call is not allowed,
because we wouldn't know then the resulting type (is it an
empty array of floats? empty array of integers? etc.)
Don't worry, you can use <tt>array_set_count(my_array, 0)</tt> for making
array empty.</p>

<p>Note that floating-point values in arrays are stored only with single-
or double- precision. This contrasts with singleton values, which are always stored
in the best precision possible. Having explicit single-
or double- precision arrays is better for storage and allows faster
copying between VRML fields. Normal <tt>array</tt> with float parameters will create
an array of single-precision values (that is, VRML <tt>MFFloat</tt>).
You have to call <?php func('array_d', 'array_d'); ?> to request double-precision storage
(suitable for VRML <tt>MFDouble</tt> or <tt>MFTime</tt>).</p>

<p><?php func('array_get_count', 'array_get_count(my_array)'); ?> and
<?php func('array_set_count', 'array_set_count(my_array, new_count)'); ?> get and set array count.
When you grow array, newly added items have undefined values.
When you shrink array, excessive values are discarded.</p>

<p><?php echo func('array_get', 'array_get(my_array, index)'); ?>
 gets an item from array on given index. In "normal" programming languages,
implemented by less lazy programmers, this is written as <tt>my_array[index]</tt> :)
Analogous
<?php echo func('array_set', 'array_set(my_array, index, component_value)'); ?>
 sets a value of item in an array.
In "normal" programming languages you would write <tt>my_array[index] := component_value</tt>.

<p><tt>array_set</tt> and <tt>array_set_count</tt> also return the new array
(that is, they return the new value of their 1st argument),
this may be comfortable sometimes.</p>

<p>You can glue (concatenate) two or more arrays by the "+" operator.</p>


<?php echo $toc->html_section(); ?>

<p><?php func('vector', 'vector(x, y), vector(x, y, z), vector(x, y, z, w)'); ?>
 create a single-precision vectors (called <tt>SFVec2f</tt>,
<tt>SFVec3f</tt>, <tt>SFVec4f</tt> in VRML).
Suffix <tt>_d</tt> means that you want double-precision vectors:
<?php func('vector_d', 'vector_d(x, y), vector_d(x, y, z), vector_d(x, y, z, w)'); ?>.</p>

<p><?php echo func('vector_get', 'vector_get(my_vec, index)'); ?>
 gets vector component. Allowed index values obviously depend on vector size,
for example on <tt>SFVec3f</tt> you can use index 0, 1, 2.
<?php echo func('vector_set', 'vector_set(my_vec, index, component_value)'); ?>
 sets given vector component (and returns new vector, for comfort).</p>

<p><?php func('vector_get_count', 'vector_get_count(my_vec)'); ?> is available,
for analogy with <tt>array_get_count</tt>. Vector has a fixed number
of components, so there is no <tt>vector_set_count</tt>.
</p>

<p>Standard vector math utilities are available:
<?php func('vector_length(v)', 'vector_length(v)'); ?>, <?php func('vector_sqr_length(v)', 'vector_sqr_length(v)'); ?>,
<?php func('vector_dot(v1, v2)', 'vector_dot(v1, v2)'); ?>  (see <a href="http://en.wikipedia.org/wiki/Dot_product">vector dot product in wikipedia</a>),
<?php func('vector_cross(v1, v2)', 'vector_cross(v1, v2)'); ?> (see <a href="http://en.wikipedia.org/wiki/Cross_product">vector cross product in wikipedia</a>,
only on 3-component vectors).
<?php func('max', 'max(vector1, vector2)'); ?>,
<?php func('min', 'min(vector1, vector2)'); ?>
 also work (make max/min on corresponding vector components).

<p>You can also add, subtract, multiply by scalar, divide by scalar,
compare vectors by normal operators.</p>

<p>Color functions: <?php func('grayscale', 'grayscale(v)'); ?>
 takes a vec3f, treats it
as RGB color, and converts it to a single float &mdash; color intensity
(calculated much like an average of vector components, but taking into
account human eye sensitivity).

<p>Note that VRML rotations (<tt>SFRotation</tt>, or an element of
<tt>MFRotation</tt> array) are, in KambiScript, just 4-value single-precision
vectors. First three items are rotation axis (should be always normalized,
VRML requires this), 4th item is the rotation angle (in radians).
So you can operate on rotations from KambiScript using all normal functions
on vectors. For now, there are no functions specialized in rotation
processing, but they may be added (we have quaternions covered in our engine)
&mdash; report if needed.</p>

<?php echo $toc->html_section(); ?>

<p>3x3 and 4x4 matrices are supported. Single- and double- precision.
VRML calls these matrix types <tt>SFMatrix3f</tt>, <tt>SFMatrix4f</tt>,
<tt>SFMatrix3d</tt>, <tt>SFMatrix4d</tt>.
Matrix is treated similar to an array of vectors (array of columns).</p>

<p><?php func('matrix', 'matrix(column1, column2, column3), matrix(column1, column2, column3, column4)'); ?>
 create a matrix. Each <tt>column</tt> argument is a vector.
Number or arguments determines if it's 3x3 or 4x4 matrix.
Type of arguments (single- or double- precision vectors) determines
if matrix is single or double precision.

<p><?php echo func('matrix_get', 'matrix_get(my_matrix, column_index)'); ?>
 gets matrix column. Allowed index values obviously depend on matrix size,
for example on <tt>SFMatrix4f</tt> you can use index 0, 1, 2, 3.
<?php echo func('matrix_set', 'matrix_set(my_matrix, column_index, column_value)'); ?>
 sets given matrix column (and returns new matrix, for comfort).</p>

<p><?php func('matrix_get_count', 'matrix_get_count(my_vec)'); ?> is available,
for analogy with <tt>array_get_count</tt> and <tt>vector_get_count</tt>.
Returns number of columns, 3 or 4. For now, non-uniform matrices are not
supported, so this is also the number of rows.</p>

<p>You can add, subtract, negate, multiply (by another matrix, or by scalar,
or by vector on the right side), divide (by scalar),
compare matrix using normal operators.</p>

<?php echo $toc->html_section(); ?>

<p><?php func('image', 'image(width, height, components)'); ?> creates
a new image. <tt>components</tt> is the number of image components,
like in VRML <tt>SFImage</tt> field:

<ul>
  <li>1 component is grayscale image,
  <li>2 components is grayscale image with alpha channel,
  <li>3 components is RGB image,
  <li>4 components is RGB image with alpha channel.
</ul>

<p>Note that image contents are <i>not initialized</i> (meaning:
they are filled with random garbage in memory) by <tt>image</tt> function.
This is for the sake of speed.</p>

<p><?php func('image_load', 'image_load(url)'); ?> loads
an image from file. This is quite powerful utility, allowing you
to load textures at any time from a script. (It's not a security
problem, since you can do the same from normal VRML nodes like <tt>ImageTexture</tt>.)
URL may be relative to VRML file containing the Script node.</p>

<p><?php func('image_width', 'image_width(my_image)'); ?>,
<?php func('image_height', 'image_height(my_image)'); ?>,
<?php func('image_components', 'image_components(my_image)'); ?> return
width, height and number of image components.</p>

<p>For functions that get/set image contents, there are 3 variants of each
of them:

<ul>
  <li><p>Functions with <tt>_color</tt> suffix operate only on non-alpha channels
    of the image. For 1 and 2 component images, they take/return
    a single floating point value describing color intensity
    (in 0..1 range). For 3 and 4 component images, they take/return
    a 3-element vector with single precision, describing RGB color value.</p></li>

  <li><p>Functions with <tt>_alpha</tt> operate only on alpha channel
    of the image. They take/return a single floating point value
    describing alpha (opacity), in 0..1 range.</p></li>

  <li><p>Finally functions without alpha/color suffix operate on all image
    channels at once. For 1 component images, they take/return
    a single floating point value. For 2,3,4 component images,
    they take/return a vector (with single precision) describing
    color with alpha value. For images without alpha value (1 or 3
    components), these functions are exactly equivalent to <tt>_color</tt>
    functions.</p></li>
</ul>

<p>Functions to get/set image contents:

<ul>
  <li><p><?php func('image_get',  'image_get&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(my_image, x, y)'); ?>,<br/>
    <?php func('image_get_color', 'image_get_color(my_image, x, y)'); ?>,<br/>
    <?php func('image_get_alpha', 'image_get_alpha(my_image, x, y)'); ?><br/>
    Get single pixel's color/alpha.</p></li>

  <li><p><?php func('image_set',  'image_set&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(my_image, x, y, color_with_alpha)'); ?>,<br/>
    <?php func('image_set_color', 'image_set_color(my_image, x, y, color)'); ?>,<br/>
    <?php func('image_set_alpha', 'image_set_alpha(my_image, x, y, alpha)'); ?><br/>
    Set single pixel to given color/alpha.</p></li>

  <li><p>More "set" functions were planned, like <tt>image_set_rectangle</tt>,
    <tt>image_apply_decal</tt>, but finally I didn't have the nerve
    to implement everything possible :) Report if you would like any function to be added
    to KambiScript for images.</p></li>
</ul>

<p>For comfort, <tt>set</tt> functions return back the image (that is,
the new value of 1st argument).</p>

<p>For example KambiScript programs that generate and process images,
see e.g. <a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_game_engine/examples/kambiscript/mkimage_gradient.kscript">mkimage_gradient.kscript
(generate simple gradient image)</a> and
<a href="https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine/trunk/kambi_vrml_game_engine/examples/kambiscript/mkimage_sobel_edge.kscript">mkimage_sobel_edge.kscript
(process any image with Sobel operator (edge detection))</a>.

<?php echo $toc->html_section(); ?>

<p>None for now. Currently, you cannot process VRML nodes directly by
KambiScript. Whether it will ever be allowed in KambiScript, depends
on the "success" of KambiScript &mdash; if you write your own scripts
in KambiScript and feel that you need this, please report. Michalis
will be more than happy to add them :)

<?php echo $toc->html_section(); ?>

<pre class="light_bg">
  Operand (aka "something that can be assigned") = Identifier

  Factor = Operand |
           Constant |
           "-" Factor |
           "(" Expression ")" |
           FunctionName [ "(" Expression [{"," Expression}] ")" ]
  FactorOperator = "^" | "*" | "/" | "%"

  # In other words, all multiplicative operators have the same priority
  # and are left-associative.
  # "^" operator is for power.
  #     X ^ Y = Power(X, Y)
  #     This works for non-integer Y, but in this case Y has to be &gt;= 0.
  # "%" operator is for modulo (remainder of division).
  #     X % Y = X - Floor(X/Y) * Y
  # "/" does division. Like in C, when both operands are integers,
  #     this performs an integer division (that is, it's the floor of
  #     actual division result, corresponding to Pascal's "div" operator).
  #     When either operand is float then this is normal float division
  #     (more precisely, if only one operand is float, the other will be
  #     promoted to float then; and then float division will be done.)

  Term = Factor [{FactorOperator Factor}]
  TermOperator = "+" | "-"

  ComparisonArgument = Term [{TermOperator Term}]
  ComparisonOperator = "&lt;" | "&gt;" | "&lt;=" | "&gt;=" | "=" | "&lt;&gt;"

  # Note that comparisons on float types (this also includes vectors, matrices
  # and arrays based on float types) perform <i>exact</i> comparison
  # (like in all programming languages).
  # This means that adding 1.0 one hundred times will not necessarily yield result
  # equal to literal 100.0. You can compare with some epsilon, like
  # "abs(a-b) < 0.001", if needed.

  NonAssignmentExpression = ComparisonArgument [{ComparisonOperator ComparisonArgument}] |

  # Programmers using our engine: note that KambiScriptParser.ParseFloatExpression
  # parses exactly "NonAssignmentExpression" token, as defined above,
  # with the Factor definition hacked to also allow only NonAssignmentExpression
  # inside parenthesis. In other words, ParseFloatExpression takes care to only
  # parse a calculated expression, without any assignments or sequence.

  PossiblyAssignmentExpression = NonAssignmentExpression |
                                 Operand ":=" PossiblyAssignmentExpression

  Expression = PossiblyAssignmentExpression [{";" PossiblyAssignmentExpression}]

  Function = "function" "(" [Identifier [{"," Identifier}] ")" Expression
  Program = [{Function}]

  # Programmers using our engine: note that KambiScriptParser.ParseProgram
  # parses exactly "Program" token defined above.

  # ------------------------------------------------
  # Programmers using our engine: note that above part of the grammar
  # was handled inside KambiScriptParser. Grammar below is handled
  # inside KambiScriptLexer.
  # A "token" returned by KambiScriptLexer corresponds to a non-terminal
  # symbol in the part of the grammar below, resolved by lexer.

  # Identifier is just a sequence of letters, underscores, digits,
  # not starting with a digit.
  Identifier = Letter [{Letter | Digit}]
  Letter = 'a' .. 'z' | 'A' .. 'Z' | "_"
  Digit = '0' .. '9'

  Constant = "pi" | "enat" |
             Digit [{Digit}] ["." Digit [{Digit}] ] |
             "true" | "false" |
             string constant in apostrophes

  FunctionName = (see list of built-in functions above)
</pre>

<p>Generally, at least one whitespace is required between two tokens.
But there are many exceptions, when situation is unambiguous,
for example no whitespace is needed between
identifiers and parenthesis.
In case of uncertainty lexer is greedy, that is lexer tries to eat as much
characters as it can for a single token.</p>

<p>Case sensitivity: language is not case-sensitive.
That said, in the future in may be partially case-sensitive,
in places where you specify field/event names of VRML
since <i>whole VRML is case-sensitive</i>. So always specify
VRML field/event names with matching case.

<!--
(wszystko jest na wartosciach rzeczywistych;
pamietaj tez ze operator potegowania ma taki sam priorytet jak
np. mnozenie a operatory o rownym priorytecie sa obliczane od
lewej do prawej wiec np. 2*4^2 = 8^2, nie 2*16))
-->

<?php
  if (!IS_GEN_LOCAL) {
    php_counter("kambi_script", TRUE);
  };

  common_footer();
?>
