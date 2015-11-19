(function($) {

    $.fn.editableZK = function(options) {

        var field = this.attr('id') || 'dummy';
        var modalId = field + '-modal';

        if ($('#' + modalId).length == 0) {
          // Merge with options from attributes.
          var settings = $.extend({
              type: this.attr('data-type'),
              pk: this.attr('data-pk'),          // primary key in the database
              url: this.attr('data-url'),        // url to send the POST request to
              title: this.attr('data-title'),    // title of the popup (will default to 'id')
              value: this.attr('data-value')     // raw value (if single input)
          }, options);

          // TODO: Sanitize inputs!

          var select2Fields = new Array();

          var formatInput = function(type, id, value, settings) {
            if (type === "text") {

              if (settings.placeholder !== undefined)
                var ph = ' placeholder="' + settings.placeholder + '"';
              else
                var ph = '';
              if (typeof(settings.toInput) === 'function')
                value = settings.toInput(value);
              return '<input type="text" class="form-control" id="' + id + '" value="' + value + '"' + ph + '>'

            } else if (type === "select") {

              select2Fields.push(id);

              var htmlArr = new Array();
              htmlArr.push('<select class="form-control" id="' + id + '">');
              for (var i = 0; i < settings.source.length; i++) {
                var selected = (settings.source[i].value == value ? ' selected' : '');
                htmlArr.push('<option value="' + settings.source[i].value + '"' + selected + '>' + settings.source[i].text + '</option>');
              };
              htmlArr.push('</select>');
              return htmlArr.join('');
            }
            return '';
          }

          if (!settings.title)
            settings.title = field;

          this.addClass('editable editable-click');
          this.attr('data-toggle', 'modal');
          this.attr('data-target', '#' + modalId);

          if (typeof(settings.display) === 'function')
            this.html(settings.display(settings.value));
          else if (typeof settings.emptytext == 'string' && settings.value === '')
            this.html(settings.emptytext);
          else
            this.html(settings.value);

          var okBtnId = field + '-ok-btn';

          var htmlArr = new Array();
          htmlArr.push('<div class="modal fade" id="' + modalId + '" tabindex="-1" role="dialog" aria-labelledby="' + modalId + '-label" aria-hidden="true">');
          htmlArr.push('  <div class="modal-dialog modal-sm">');
          htmlArr.push('    <div class="modal-content">');
          htmlArr.push('      <div class="modal-header">');
          htmlArr.push('        <button type="button" class="close" data-dismiss="modal" aria-hidden="true">&times;</button>');
          htmlArr.push('        <h4 class="modal-title" id="' + modalId + '-label">' + settings.title + '</h4>');
          htmlArr.push('      </div>');
          htmlArr.push('      <div class="modal-body">');
          htmlArr.push('        <div class="row">');

          var firstFieldId = undefined;
          if (!settings.fields) {
            // No fields definition - create a simple editor
            firstFieldId = modalId + '-edit';
            htmlArr.push('          <div class="col-md-12">');
            htmlArr.push('            ' + formatInput(settings.type, modalId + '-edit', settings.value, settings));
            htmlArr.push('          </div>');
          } else {
            // Fields definition present - create an editor with labels
            for (var key in settings.fields) {
              var fieldId = modalId + '-' + key;

              if (firstFieldId === undefined)
                firstFieldId = fieldId;

              if (settings.fields[key].placeholder === undefined)
                settings.fields[key].placeholder = '';

              htmlArr.push('          <div class="col-md-4">');
              htmlArr.push('            <label for="' + fieldId + '">' + settings.fields[key].label + '</label>');
              htmlArr.push('          </div>');
              htmlArr.push('          <div class="col-md-8">');
              htmlArr.push('            ' + formatInput(settings.fields[key].type || settings.type,
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
            $('#' + modalId).on('show.bs.modal', function () {
              // TODO: Initialize the value, especially on reentry (and remove the surrounding IF then)!
              for (var i = select2Fields.length - 1; i >= 0; i--) {
                $("#" + select2Fields[i]).select2();
              };
            });
          }

          $('#' + modalId).on('shown.bs.modal', function () {
            $('#' + firstFieldId).focus();
          });

          $('#' + okBtnId).click(function (){
            // TODO: Write this code!
            // 1. get value
            // 2. validate value
            // 3. change element on page using display()
            $('#' + modalId).modal('hide');
          });
        }

        return this;
    };

}(jQuery));
