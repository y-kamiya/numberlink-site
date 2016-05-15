$(function () {

    var CHAR_CODE_OFFSET = 96;
    var ID_IS_NONE = -1;
    var KEYCODE = {LEFT: 37, UP: 38, RIGHT: 39, DOWN: 40};

    $(function() {
        $('#createTableButton').on('click', createTable);
        $('#save').on('click', save);
        $('#solve').on('click', initEditor);
    });

    var createTable = function() {
        var table = $('<table>'),
            rowNum = $('#rowNum').val(),
            colNum = $('#colNum').val();

        $('div.fieldSize input').attr('disabled', 'disabled');

        for (var row = 0; row < rowNum; row++) {
            var tr = $('<tr>')
            for (var col = 0; col < colNum; col++) {
                var cellId = row * colNum + col;
                tr.append("<td id=\"c" + cellId + "\"><input type=\"number\"></td>");
            }
            table.append(tr);
        }
        $('#field').append(table);

        $('#buttons div').eq(0).addClass('disabled');
        $('#buttons div').eq(1).removeClass('disabled');
    }

    var initEditor = function() {
        save();
        $('#field table').remove();
        $('#input').addClass('disabled');
        $('#buttons div').eq(1).addClass('disabled');
        $('#buttons div').eq(2).removeClass('disabled');
        initCursor();
        load();
    }

    var initCursor = function() {
        var handler = function(code) {
            return function() {
                var e = jQuery.Event('keydown', { keyCode: code });
                $(this).trigger(e);
            };
        };
        $('#cursor-up').on('click', handler(KEYCODE.UP));
        $('#cursor-left').on('click', handler(KEYCODE.LEFT));
        $('#cursor-right').on('click', handler(KEYCODE.RIGHT));
        $('#cursor-down').on('click', handler(KEYCODE.DOWN));
    }

    var save = function () {
        var rowNum = $('#rowNum').val(),
            colNum = $('#colNum').val();

        var text = "";
        for (var row = 0; row < rowNum; row++) {
            for (var col = 0; col < colNum; col++) {
                var cellId = row * colNum + col,
                    value = $('#c' + cellId + " > input").val();
                if (value == "") {
                    text += ".";
                } else {
                    text += String.fromCharCode(Number(value) + CHAR_CODE_OFFSET);
                }
            }
            text += "<br>"
        }
        $('#input').append(text);
    }


    var load = function() {
        var table = $('<table>'),
            data = $('#input').text(),
            rowNum = $('#rowNum').val(),
            colNum = $('#colNum').val();

        for (var row = 0; row < rowNum; row++) {
            var tr = $('<tr>')
            for (var col = 0; col < colNum; col++) {
                var cellId = row * colNum + col,
                    c = data[cellId];

                var innerTable = '<table class="innerTable"><tr><td></td><td></td></tr><tr><td></td><td></td></tr></table>';
                if (c == '.') {
                    tr.append("<td id=\"" + cellId + "\">" + innerTable + "</td>");
                } else {
                    tr.append("<td id=\"" + cellId + "\">" + (c.charCodeAt() - CHAR_CODE_OFFSET) + "</td>");
                }
            }
            table.append(tr);
        }
        $('#field').append(table);
        $('#field table td').on('click', start);
    }

    var currentId = ID_IS_NONE;

    var start = function() {
        $('#field td').removeClass('current');
        $(this).find('td').removeClass();
        $(this).addClass('current');
        currentId = $(this)[0].id;
    }

    var getNextId = function(id, dir) {
        var rowNum = $('#rowNum').val();
        var colNum = $('#colNum').val();
        var row = id / colNum; 
        var col = id % colNum; 
        console.log(dir, id);
        switch (dir) {
            case KEYCODE.LEFT:
                if (col - 1 < 0) return ID_IS_NONE;
                return id - 1;
            case KEYCODE.UP:
                if (row - 1 < 0) return ID_IS_NONE;
                console.log(id, colNum);
                console.log(Number(id) - Number(colNum));
                return Number(id) - Number(colNum);
            case KEYCODE.RIGHT:
                if (colNum <= col + 1) return ID_IS_NONE;
                return Number(id) + 1;
            case KEYCODE.DOWN:
                if (rowNum <= row + 1) return ID_IS_NONE;
                return Number(id) + Number(colNum);
            default:
                return ;
        }
    }

    var lineCurrentCell = function(current, to) {
        var td = current.find('td');
        switch (to) {
            case KEYCODE.LEFT:
                td.eq(2).addClass('top');
                break;
            case KEYCODE.UP:
                td.eq(0).addClass('right');
                break;
            case KEYCODE.RIGHT:
                td.eq(1).addClass('bottom');
                break;
            case KEYCODE.DOWN:
                td.eq(3).addClass('left');
                break;
        }
    }

    var lineNextCell = function(next, to) {
        var td = next.find('td');
        switch (to) {
            case KEYCODE.LEFT:
                td.eq(1).addClass('bottom');
                break;
            case KEYCODE.UP:
                td.eq(3).addClass('left');
                break;
            case KEYCODE.RIGHT:
                td.eq(2).addClass('top');
                break;
            case KEYCODE.DOWN:
                td.eq(0).addClass('right');
                break;
        }
    }

    var hasTable = function(elem) {
        if (0 < elem.find('table').length) return true;
        return false;
    }

    $('body').keydown(function(e) {
        if (currentId == ID_IS_NONE) {
            return;
        }
        var rowNum = $('rowNum').val();
        var current = $('#' + currentId);
        var nextId = getNextId(currentId, e.keyCode);
        console.log(currentId);
        console.log(nextId);
        if (nextId == ID_IS_NONE) {
            return;
        }

        current.removeClass('current');

        if (hasTable(current)) {
            lineCurrentCell(current, e.keyCode);
        }

        var next = $('#' + nextId);

        if (hasTable(next)) {
            lineNextCell(next, e.keyCode);
            next.addClass('current');
            currentId = nextId;
        } else {
            currentId = ID_IS_NONE; 
        }

    });

});

