<?php
/*************************************************************************************
 * vrmlx3d.php
 * --------
 * Author: Michalis Kamburelis, based on Bash by Andreas Gohr (andi@splitbrain.org)
 * Copyright: 2016 Michalis Kamburelis, and see bash.php copyright: (c) 2004 Andreas Gohr, Nigel McNie (http://qbnz.com/highlighter)
 *
 *************************************************************************************
 *
 *   This file is not (yet) part of GeSHi,
 *   it was added for Castle Game Engine website.
 *
 *   It is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 ************************************************************************************/

$language_data = array (
    'LANG_NAME' => 'VRML / X3D',
    // Bash DOES have single line comments with # markers. But bash also has
    // the  $# variable, so comments need special handling (see sf.net
    // 1564839)
    'COMMENT_SINGLE' => array('#'),
    'COMMENT_MULTI' => array(),
    'COMMENT_REGEXP' => array(
        //Variables
        1 => "/\\$\\{[^\\n\\}]*?\\}/i",
        //BASH-style Heredoc
        2 => '/<<-?\s*?(\'?)([a-zA-Z0-9]+)\1\\n.*\\n\\2(?![a-zA-Z0-9])/siU',
        //Escaped String Starters
        3 => "/\\\\['\"]/siU",
        // Single-Line Shell usage: Hide the prompt at the beginning
        /* 4 => "/\A(?!#!)\s*(?>[\w:@\\/\\-\\._~]*[$#]\s?)?(?=[^\n]+\n?\Z)|^(?!#!)(\w+@)?[\w\\-\\.]+(:~?)[\w\\/\\-\\._]*?[$#]\s?/ms" */
        4 => "/\A(?!#!)(?:(?>[\w:@\\/\\-\\._~]*)[$#]\s?)(?=(?>[^\n]+)\n?\Z)|^(?!#!)(?:\w+@)?(?>[\w\\-\\.]+)(?>:~?[\w\\/\\-\\._]*?)?[$#]\s?/sm"
        ),
    'CASE_KEYWORDS' => GESHI_CAPS_NO_CHANGE,
    'QUOTEMARKS' => array('"'),
    'HARDQUOTE' => array("'", "'"),
    'HARDESCAPE' => array("\'"),
    'ESCAPE_CHAR' => '',
    'ESCAPE_REGEXP' => array(
        //Simple Single Char Escapes
        1 => "#\\\\[nfrtv\\$\\\"\n]#i",
        // $var
        2 => "#\\$[a-z_][a-z0-9_]*#i",
        // ${...}
        3 => "/\\$\\{[^\\n\\}]*?\\}/i",
        // $(...)
        4 => "/\\$\\([^\\n\\)]*?\\)/i",
        // `...`
        5 => "/`[^`]*`/"
        ),
    'KEYWORDS' => array(
        1 => array(
            // see castle_game_engine/src/x3d/x3dlexer.pas
            // for VRML / X3D keywords list
            'DEF', 'USE', 'ROUTE', 'TO',
            'EXTERNPROTO', 'FALSE', 'IS', 'NULL', 'PROTO', 'TRUE',
            'AS', 'EXPORT', 'IMPORT',
            'COMPONENT', 'META', 'PROFILE', 'UNIT',
            ),
        2 => array(
            'eventIn', 'eventOut', 'exposedField', 'field',
            'inputOnly', 'outputOnly', 'inputOutput', 'initializeOnly',
            ),
        3 => array(
            )
        ),
    'SYMBOLS' => array(
        '(', ')', '[', ']', '!', '@', '%', '&', '*', '|', '/', '<', '>', ';;', '`'
        ),
    'CASE_SENSITIVE' => array(
        GESHI_COMMENTS => false,
        1 => true,
        2 => true,
        3 => true
        ),
    'STYLES' => array(
        'KEYWORDS' => array(
            1 => 'color: #000000; font-weight: bold;',
            2 => 'color: #c20cb9; font-weight: bold;',
            3 => 'color: #7a0874; font-weight: bold;'
            ),
        'COMMENTS' => array(
            0 => 'color: #666666; font-style: italic;',
            1 => 'color: #800000;',
            2 => 'color: #cc0000; font-style: italic;',
            3 => 'color: #000000; font-weight: bold;',
            4 => 'color: #666666;'
            ),
        'ESCAPE_CHAR' => array(
            1 => 'color: #000099; font-weight: bold;',
            2 => 'color: #007800;',
            3 => 'color: #007800;',
            4 => 'color: #007800;',
            5 => 'color: #780078;',
            'HARD' => 'color: #000099; font-weight: bold;'
            ),
        'BRACKETS' => array(
            0 => 'color: #7a0874; font-weight: bold;'
            ),
        'STRINGS' => array(
            0 => 'color: #ff0000;',
            'HARD' => 'color: #ff0000;'
            ),
        'NUMBERS' => array(
            0 => 'color: #000000;'
            ),
        'METHODS' => array(
            ),
        'SYMBOLS' => array(
            0 => 'color: #000000; font-weight: bold;'
            ),
        'REGEXPS' => array(
            0 => 'color: #007800;',
            1 => 'color: #007800;',
            2 => 'color: #007800;',
            4 => 'color: #007800;',
            5 => 'color: #660033;'
            ),
        'SCRIPT' => array(
            )
        ),
    'URLS' => array(
        1 => '',
        2 => '',
        3 => ''
        ),
    'OOLANG' => false,
    'OBJECT_SPLITTERS' => array(
        ),
    'REGEXPS' => array(
        //Variables (will be handled by comment_regexps)
        0 => "\\$\\{[a-zA-Z_][a-zA-Z0-9_]*?\\}",
        //Variables without braces
        1 => "\\$[a-zA-Z_][a-zA-Z0-9_]*",
        //Variable assignment
        2 => "(?<![\.a-zA-Z_\-])([a-zA-Z_][a-zA-Z0-9_]*?)(?==)",
        //Shorthand shell variables
        4 => "\\$[*#\$\\-\\?!\d]",
        //Parameters of commands
        5 => "(?<=\s)--?[0-9a-zA-Z\-]+(?=[\s=]|<(?:SEMI|PIPE)>|$)"
        ),
    'STRICT_MODE_APPLIES' => GESHI_NEVER,
    'SCRIPT_DELIMITERS' => array(
        ),
    'HIGHLIGHT_STRICT_BLOCK' => array(
        ),
    'TAB_WIDTH' => 4,
    'PARSER_CONTROL' => array(
        'COMMENTS' => array(
            'DISALLOWED_BEFORE' => '$'
        ),
        'KEYWORDS' => array(
            'DISALLOWED_BEFORE' => "(?<![\.\-a-zA-Z0-9_\$\#:])",
            'DISALLOWED_AFTER' =>  "(?![\.\-a-zA-Z0-9_%=\\/:])",
            2 => array(
                'SPACE_AS_WHITESPACE' => false
                )
            )
        )
);

?>