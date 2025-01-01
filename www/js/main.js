// www/js/main.js
$(document).ready(function() {
  // Handle regular toggles (non-property section)
  function initializeRegularToggles() {
      $('.collapse-toggle').each(function() {
          var $button = $(this);
          var targetId = $button.next('.collapse').attr('id');
          
          $button.off('click').on('click', function() {
              var $target = $('#' + targetId);
              var isCollapsed = !$target.hasClass('show');
              
              if (isCollapsed) {
                  $target.collapse('show');
                  $button.text('Hide additional parameters');
              } else {
                  $target.collapse('hide');
                  $button.text('Show more parameters');
              }
          });
      });
  }

  // Initialize on page load
  initializeRegularToggles();

  // Re-initialize after Shiny updates
  $(document).on('shiny:value', function() {
      setTimeout(initializeRegularToggles, 100);
  });

  // Re-initialize after any Shiny binding
  $(document).on('shiny:bound', function() {
      setTimeout(initializeRegularToggles, 100);
  });
});