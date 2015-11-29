(function($) {

    $.fn.editableZK = function(options) {

        var field = this.attr('id') || 'dummy';
        var modalId = field + '-modal';
        var errorId = field + '-error';
        var okBtnId = field + '-ok-btn';
        var target = this;

        if ($('#' + modalId).length == 0) {
          // Merge with options from attributes.
          var settings = $.extend({
              type: target.attr('data-type'),
              pk: target.attr('data-pk'),                    // primary key in the database
              url: target.attr('data-url'),                  // url to send the POST request to
              title: target.attr('data-title'),              // title of the popup (will default to 'id')
              value: target.attr('data-value'),              // raw value (if single input)
              emptytext: target.attr('data-emptytext'),      // text to display when the value is empty
              placeholder: target.attr('data-placeholder'),  // placeholder for text values
              rows: target.attr('data-rows')                 // number of rows for text areas
          }, options);

          // TODO: Sanitize inputs!

          var select2Fields = new Array();

          var formatInput = function(type, id, value, settings) {
            return formatInputImpl(type, id, typeof(settings.toInput) === 'function' ? settings.toInput(value) : value, settings);
          }

          var formatInputImpl = function(type, id, value, settings) {
            if (type === 'text') {

              if (settings.placeholder !== undefined)
                var ph = ' placeholder="' + settings.placeholder + '"';
              else
                var ph = '';
              return '<input type="text" class="form-control" id="' + id + '" value="' + value + '"' + ph + '>';

            } else if (type === 'select') {

              select2Fields.push({id: id});

              var htmlArr = new Array();
              htmlArr.push('<select class="form-control" id="' + id + '" style="width: 100%">');
              for (var i = 0; i < settings.source.length; i++) {
                var selected = (settings.source[i].value == value ? ' selected' : '');
                htmlArr.push('<option value="' + settings.source[i].value + '"' + selected + '>' + settings.source[i].text + '</option>');
              };
              htmlArr.push('</select>');

              return htmlArr.join('');

            } else if (type === 'select2') {

              select2Fields.push({id: id, options: settings.select2});

              return '<input type="text" class="form-control" id="' + id + '" value="' + value + '">';

            } else if (type === 'textarea') {

              if (settings.rows !== undefined)
                var rows = ' rows="' + settings.rows + '"';
              else
                var rows = '';
               return '<textarea class="form-control" id="' + id + '"' + rows + '>' + value + '</textarea>';

            } else {

              return '';

            }
          }

          if (!settings.title)
            settings.title = field;

          target.addClass('editable editable-click');
          target.attr('data-toggle', 'modal');
          target.attr('data-target', '#' + modalId);

          if (typeof(settings.display) === 'function') {
            target.html(settings.display(settings.value));
          } else if (typeof settings.emptytext == 'string' && settings.value === '') {
            target.html(settings.emptytext);
          } else if (settings.source != undefined && settings.source.length != undefined) {
            for (var i = settings.source.length - 1; i >= 0; i--) {
              if (settings.source[i].value == settings.value)
                target.html(settings.source[i].text);
            }
          } else if (settings.select2 != undefined) {
            var values = settings.value.split("||");
            var data = new Array;
            for (var i = values.length - 1; i >= 0; i -= 2) {
              data.push(values[i]);
            };
            target.html(data.join(', '));
          } else {
            target.html(settings.value);
          }

          var htmlArr = new Array();
          htmlArr.push('<div class="modal fade" id="' + modalId + '" tabindex="-1" role="dialog" aria-labelledby="' + modalId + '-label" aria-hidden="true">');
          htmlArr.push('  <div class="modal-dialog modal-sm">');
          htmlArr.push('    <div class="modal-content">');
          htmlArr.push('      <div class="modal-header">');
          htmlArr.push('        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>');
          htmlArr.push('        <h4 class="modal-title" id="' + modalId + '-label">' + settings.title + '</h4>');
          htmlArr.push('      </div>');
          htmlArr.push('      <div id="' + errorId + '" class="alert alert-danger" role="alert" style="display: none;"></div>');
          htmlArr.push('      <div class="modal-body">');
          htmlArr.push('        <div class="row">');

          var fieldInfo = new Array;
          if (!settings.fields) {
            // No fields definition - create a simple editor
            fieldInfo.push({id: modalId + '-edit', type: settings.type});

            htmlArr.push('          <div class="col-md-12">');
            htmlArr.push('            ' + formatInput(settings.type, fieldInfo[0].id, settings.value, settings));
            htmlArr.push('          </div>');
          } else {
            // Fields definition present - create an editor with labels
            for (var key in settings.fields) {
              var fieldId = modalId + '-' + key;
              var fieldType = settings.fields[key].type || settings.type;

              fieldInfo.push({id: fieldId, name: key, type: fieldType});

              if (settings.fields[key].placeholder === undefined)
                settings.fields[key].placeholder = '';

              htmlArr.push('          <div class="col-md-4">');
              htmlArr.push('            <label for="' + fieldId + '">' + settings.fields[key].label + '</label>');
              htmlArr.push('          </div>');
              htmlArr.push('          <div class="col-md-8">');
              htmlArr.push('            ' + formatInput(fieldType,
                                                        fieldId,
                                                        typeof settings.value[key] == 'string' ? settings.value[key] : settings.value,
                                                        settings.fields[key]));
              htmlArr.push('          </div>');
            }
          }

          htmlArr.push('        </div>');
          htmlArr.push('      </div>');
          htmlArr.push('      <div class="modal-footer">');
          htmlArr.push('        <button type="button" class="btn btn-xs btn-default" data-dismiss="modal">Anuluj</button>');
          htmlArr.push('        <button type="button" class="btn btn-xs btn-zk" id="' + okBtnId + '">OK</button>');
          htmlArr.push('      </div>');
          htmlArr.push('    </div>');
          htmlArr.push('  </div>');
          htmlArr.push('</div>');

          $('body').append(htmlArr.join(''));

          if (select2Fields.length > 0) {
            var initHandler = function () {
              // Initialize select2 controls.
              for (var i = select2Fields.length - 1; i >= 0; i--) {
                var q = '#' + select2Fields[i].id;
                if (select2Fields[i].options !== undefined)
                  $(q).select2(select2Fields[i].options);
                else
                  $(q).select2();
              }
              // Only do it the first time around.
              $('#' + modalId).unbind('show.bs.modal', initHandler);
            };
            $('#' + modalId).on('show.bs.modal', initHandler);
          }

          $('#' + modalId).on('shown.bs.modal', function () {
            $('#' + modalId).data('okFlag', false);

            $('#' + fieldInfo[0].id).focus();

            if (fieldInfo[0].type.indexOf('select') == 0) {
              $('#' + fieldInfo[0].id).select2("open");
            }
          });

          $('#' + modalId).on('hidden.bs.modal', function () {
            if ($('#' + modalId).data('okFlag') === false) {
              var setInput = function(type, id, value) {
                if (type === 'text' || type === 'textarea') {
                  $('#' + id).val(value);
                } else if (type === 'select') {
                  $('#' + id).select2('data', { value: 'Kolekcja', text: 'Kolekcja wydawnicza' });
                  $('#' + id).val('Kolekcja');
                } else if (type === 'select2') {
                  $('#' + id).select2('data', [{value: '2', text: 'Lolek'}, {value: '3', text: 'Bolek'}]);
                  $('#' + id).val('2,3');
                }
              };

              if (!settings.fields) {
                setInput(settings.type, fieldInfo[0].id, settings.value);
              } else {
                for (var i = fieldInfo.length - 1; i >= 0; i--) {
                  var key = fieldInfo[i].name;
                  setInput(fieldInfo[i].type,
                           fieldInfo[i].id,
                           typeof settings.value[key] == 'string' ? settings.value[key] : settings.value);
                }
              }
            }

            $('#' + modalId).data('okFlag', false);
          });

          var enterHandler = function(e) {
            if (e.which == 13)
              $('#' + okBtnId).click();
          };
          for (var i = fieldInfo.length - 1; i >= 0; i--) {
            // select2 controls automatically open on Enter - and prevents the Esc key from being caught...
            if (fieldInfo[i].type.indexOf('select') == -1)
              $('#' + fieldInfo[i].id).keyup(enterHandler);
          };

          $('#' + okBtnId).click(function (){
            // 1. Get the value
            var getRawVal = function(fieldInfo) {
              return $('#' + fieldInfo.id).val();
            }
            var getTextVal = function(fieldInfo, rawVal) {
              if (fieldInfo.type == 'select') {
                return $('#' + fieldInfo.id).select2('data').text;
              } else if (fieldInfo.type == 'select2') {
                var arr = new Array;
                var selData = $('#' + fieldInfo.id).select2('data');
                for (var i = 0; i < selData.length; i++) {
                  arr.push(selData[i].text);
                };
                return arr.join(', ');
              } else if (fieldInfo.type == 'textarea') {
                return rawVal.replace('\n', '<br>');
              } else {
                return rawVal;
              }
            }

            if (fieldInfo.length > 1) {
              var rawVal = {};
              var textVal = {};
              for (var i = fieldInfo.length - 1; i >= 0; i--) {
                var thisRawVal = getRawVal(fieldInfo[i]);
                rawVal[fieldInfo[i].name] = thisRawVal;
                textVal[fieldInfo[i].name] = getTextVal(fieldInfo[i], thisRawVal);
              }
            } else {
              var thisRawVal = getRawVal(fieldInfo[0]);
              rawVal = thisRawVal;
              textVal = getTextVal(fieldInfo[0], thisRawVal);
            }

            // 2. translate (if required) and validate value
            var rawVal2 = typeof(settings.fromInput) === 'function' ? settings.fromInput(rawVal) : rawVal;
            var errorMsg = typeof(settings.validate) === 'function' ? settings.validate(rawVal2) : '';

            if (typeof errorMsg === 'string' && errorMsg != '') {
              // Display the error message
              $('#' + errorId).html(errorMsg).show();
            } else {
              // 3. TODO: send request to the server

              // 4. change element on page using display()
              if(typeof(settings.display) === 'function') {
                target.html(settings.display(rawVal2, textVal));
              } else {
                // This won't work properly if this is a multi-field editor - and that's what we want
                // because in such cases the user should provide the display() function.
                target.html(textVal);
              }

              // 5. hide the modal
              $('#' + modalId).data('okFlag', true);
              $('#' + modalId).modal('hide');
              $('#' + errorId).hide();
            }
          });
        }

        return this;
    };

}(jQuery));
