function plotSelect() {
        var plotIndex = document.getElementById("type").selectedIndex;
        var plotName = document.getElementById("type")[plotIndex].value;
        var lookbackDays = document.getElementById("lookbackDays");
        var pinColor = document.getElementById("pinColor");
        var mapType = document.getElementById("mapType");
        var size = document.getElementById("size");
        if (plotName == "dailyBarplot") {
          lookbackDays.disabled = false;
          pinColor.disabled = true;
          mapType.disabled = true;
          size.options["small"].value = "500";
            size.options["large"].value = "700";
        } else if (plotName == "dailyByHour") {
            lookbackDays.disabled = false;
            pinColor.disabled = true;
            mapType.disabled = true;
            size.options["small"].value = "500";
            size.options["large"].value = "700";
        } else if (plotName == "timeseries") {
             lookbackDays.disabled = false;
              pinColor.disabled = true;
              mapType.disabled = true;
              size.options["small"].value = "500";
            size.options["large"].value = "700";
        } else if (plotName == "locationMap") {
              lookbackDays.disabled = true;
              pinColor.disabled = false;
              mapType.disabled = false;
              size.options["small"].value = "250";
            size.options["large"].value = "500";
        }
    }
