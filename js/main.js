(function($) {
  $(document).ready(function() {
    $('article code.hljs').each(function() {
      $(this).parent('.sourceCode').prepend($('<div class="lang"/>').append(this.result.language));
    });
  });
})(jQuery);
