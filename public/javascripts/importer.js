$(function () {
  var $form = $("main.importer form");
  $form.submit(function () {
    setTimeout(function () {
      $form.html(lishogi.spinnerHtml);
    }, 50);
  });

  if (window.FileReader) {
    $form.find("input[type=file]").on("change", function () {
      var file = this.files[0];
      if (!file) return;
      var reader = new FileReader();

      reader.onload = function (e) {
        var codes = new Uint8Array(e.target.result);
        var unicodeString = Encoding.convert(codes, {
          to: 'unicode',
          from: 'auto',
          type: 'string'
        });
        $form.find("input[type=hidden]").change(function() {
          $form.find("textarea").val(unicodeString);
        }).val(kifToPgn(unicodeString)).change();
      };
      reader.readAsArrayBuffer(file);
    });
  } else $form.find(".upload").remove();

  var kifToPgn = function(kif) {
    return kif;
  }
});
