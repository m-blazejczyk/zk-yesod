(function($) {

    $.fn.editableZK = function(options) {

        var field = this.attr('id');
        var modalId = field + '-modal';

        if ($('#' + modalId).length == 0) {
          // Merge with options from attributes.
          var settings = $.extend({
              type: this.attr('data-type'),
              pk: this.attr('data-pk'),
              url: this.attr('data-url'),
              title: this.attr('data-title') || '',
              value: this.attr('data-value')
          }, options);

          this.addClass('editable editable-click');
          this.attr('data-toggle', 'modal');
          this.attr('data-target', '#' + modalId);

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
          htmlArr.push('          <div class="col-md-4">');
          htmlArr.push('            <label for="exampleInputEmail1">Obrazek</label>');
          htmlArr.push('          </div>');
          htmlArr.push('          <div class="col-md-8">');
          htmlArr.push('            <input type="text" class="form-control" id="exampleInputEmail1" placeholder="Obrazek">');
          htmlArr.push('          </div>');
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
