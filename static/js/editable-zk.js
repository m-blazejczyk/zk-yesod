(function($) {

    $.fn.editableZK = function(options) {

        var field = this.attr('id');
        var modalId = field + '-modal';

        if ($('#' + modalId).length == 0) {
          // Merge with options from attributes.
          var settings = $.extend({
              type: this.attr('data-type'),
              pk: this.attr('data-pk'),                         // primary key in the database
              url: this.attr('data-url'),                       // url to send the POST request to
              title: this.attr('data-title') || '',             // title of the popup (will default to 'name')
              name: this.attr('data-name') || '',               // name of the field (if single input)
              value: this.attr('data-value'),                   // raw value (if single input)
              placeholder: this.attr('data-placeholder') || ''  // placeholder (if single input; will default to 'title')
          }, options);

          var formatInput = function(type, id, value, settings) {
            if (type === "text") {
              if (settings.placeholder !== undefined)
                var ph = ' placeholder="' + settings.placeholder + '"';
              else
                var ph = '';
              if (typeof(settings.toInput) === 'function')
                value = settings.toInput(value);
              return '<input type="text" class="form-control" id="' + id + '" value="' + value + '"' + ph + '>'
            }
            return '';
          }

          // TODO: Sanitize inputs!

          if (settings.name == '' && settings.fields === undefined)
            settings.name = 'dummy';
          if (settings.title == '')
            settings.title = settings.name;
          if (settings.placeholder == '')
            settings.placeholder = settings.title;

          this.addClass('editable editable-click');
          this.attr('data-toggle', 'modal');
          this.attr('data-target', '#' + modalId);

          if (typeof(settings.display) === 'function')
            this.html(settings.display(settings.value));
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

          if (settings.fields === undefined) {
            // No fields definition - create a simple editor
            htmlArr.push('          <div class="col-md-12">');
            htmlArr.push('            ' + formatInput(settings.type, modalId + settings.name, settings.value, settings));
            htmlArr.push('          </div>');
          } else {
            // Fields definition present - create an editor with labels
            for (var key in settings.fields) {
              var fieldId = modalId + key;

              if (settings.fields[key].placeholder === undefined)
                settings.fields[key].placeholder = '';

              htmlArr.push('          <div class="col-md-4">');
              htmlArr.push('            <label for="' + fieldId + '">' + settings.fields[key].label + '</label>');
              htmlArr.push('          </div>');
              htmlArr.push('          <div class="col-md-8">');
              htmlArr.push('            ' + formatInput(settings.fields[key].type || settings.type,
                                                        fieldId,
                                                        settings.fields[key].value || settings.value,
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

          $('#' + modalId).on('shown.bs.modal', function () {
            $('#exampleInputEmail1').focus()
          });
        }

        return this;
    };

}(jQuery));
