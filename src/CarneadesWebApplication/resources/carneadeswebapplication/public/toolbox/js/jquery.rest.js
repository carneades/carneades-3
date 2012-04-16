/*
 * Copyright 2011 Andrey “A.I.” Sitnik <andrey@sitnik.ru>,
 * sponsored by Evil Martians.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * Add shortcuts to make PUT and DELETE AJAX requests.
 * 
 * Shorcuts create POST request and override HTTP method by
 * X-HTTP-Method-Override header.
 */
;(function($) {
    var methods = {}
    $.each({ put: 'PUT', del: 'DELETE' }, function(prop, method) {
        methods[prop] = function(url, data, callback, type) {
            if ($.isFunction(data)) {
                type = type || callback
                callback = data
                data = { }
            }
            
            return $.ajax({ headers: { 'X-HTTP-Method-Override': method },
                            type:    'POST',
                            url:     url,
                            data:    data,
                            success: callback,
                            dataType: type })
        }
    })
    $.extend(methods)
})(jQuery);
