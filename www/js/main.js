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

$(document).ready(function() {  
    // Function to enforce fixed start year for the range slider
    function enforceFixedStartYear() {
      var slider = $("#global_year_range").data("ionRangeSlider");
      if (slider) {
        // Store the original update method
        var originalUpdate = slider.update;
        
        // Override the update method
        slider.update = function(options) {
          // If trying to change the from value, force it back to 2025
          if (options && typeof options.from !== 'undefined') {
            options.from = 2025;
          }
          // Call the original update method
          originalUpdate.call(this, options);
        };
      }
    }
  
    // Initialize the fixed start year enforcement
    enforceFixedStartYear();
  
    // Also handle the case when Shiny updates the slider
    $(document).on('shiny:value', function(event) {
      if (event.name === 'global_year_range') {
        setTimeout(enforceFixedStartYear, 100);
      }
    });
  });

$(document).ready(function() {
  // Initially hide just the sensitivity analysis results container
  $(".sensitivity-results").hide();
});