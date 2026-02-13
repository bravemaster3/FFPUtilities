function calc_footprint_matlab(csv_input_path, output_dir, site_long, site_lat, zm, radius_vector, ffp_function_name)
    % calc_footprint_matlab - Calculate footprint using specified FFP function
    %
    % Inputs:
    %   csv_input_path - Path to prepared EC data CSV
    %   output_dir - Directory to save output CSVs
    %   site_long - Site longitude (decimal degrees)
    %   site_lat - Site latitude (decimal degrees)
    %   zm - Measurement height (m)
    %   radius_vector - Vector of contour percentages (e.g., [50 60 70 80 90])
    %   ffp_function_name - Name of FFP function to use:
    %                       'calc_footprint_FFP_climatology' (default) or
    %                       'calc_footprint_FFP_climatology_parallel'
    
    % Set default function name if not provided
    if nargin < 7
        ffp_function_name = 'calc_footprint_FFP_climatology';
    end
    
    fprintf('Using FFP function: %s\n', ffp_function_name);
    
    % Read prepared EC data
    data = readtable(csv_input_path);
    fprintf('Read %d observations from %s\n', height(data), csv_input_path);
    
    % Prepare parameters (transpose to row vectors for MATLAB FFP function)
    z0 = NaN;
    umean = transpose(data.wind_speed);
    h = transpose(data.blh);
    ol = transpose(data.L);
    sigmav = transpose(sqrt(data.v_var));
    ustar = transpose(data.ustar);
    wind_dir = transpose(data.wind_dir);
    
    % Calculate footprint climatology using specified function
    fprintf('Calculating footprint climatology...\n');
    
    if strcmp(ffp_function_name, 'calc_footprint_FFP_climatology_parallel')
        % Use parallel version
        [FFP, flag_err] = calc_footprint_FFP_climatology_parallel(zm, z0, umean, h, ol, sigmav, ustar, wind_dir, ...
            'nx', 1100, 'r', radius_vector, 'smooth_data', 1, 'fig', 0);
    else
        % Use standard version
        [FFP, flag_err] = calc_footprint_FFP_climatology(zm, z0, umean, h, ol, sigmav, ustar, wind_dir, ...
            'nx', 1100, 'r', radius_vector, 'smooth_data', 1, 'fig', 0);
    end
    
    fprintf('Footprint calculation complete.\n');
    
    % Convert footprint contours to lat/long and save
    R = 6371007.181;  % Earth radius in meters
    
    for i = 1:length(radius_vector)
        % Get contour coordinates
        xr = FFP(i).xr';
        yr = FFP(i).yr';
        
        % Convert to lat/long
        dLat = yr ./ R;
        dLon = xr ./ (R * cos(pi * site_lat / 180));
        
        latO = site_lat + dLat * 180 / pi;
        lonO = site_long + dLon * 180 / pi;
        
        % Create table
        coord = table(xr, yr, lonO, latO, 'VariableNames', {'xr', 'yr', 'long', 'lat'});
        
        % Save to CSV
        pct = radius_vector(i);
        filename = fullfile(output_dir, sprintf('footprint_%dpct.csv', pct));
        writetable(coord, filename);
        
        fprintf('Saved %s\n', filename);
    end
    
    fprintf('All contours saved to %s\n', output_dir);
end