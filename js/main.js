(function($) {
  $(document).ready(function() {
    $('article pre.sourceCode').each(function() {
      var self = $(this);
      self.prepend($('<div class="lang"/>').append(self.attr("class").replace("sourceCode ", "")));
    });
  });
})(jQuery);
