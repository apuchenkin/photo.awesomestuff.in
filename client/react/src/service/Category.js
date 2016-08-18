import fetch from 'isomorphic-fetch';

// var url = require('url');
// var url_parts = url.parse(request.url, true);
// var query = url_parts.query;

const CategoryService = class {

  constructor(token, location) {
    this.token = token;
    this.location = location;
  }

  fetchCategories () {
    let me = this;
    // url.searchParams.append('hidden', true);

    return fetch(me.location + '/api/v1/category', {
        headers: {
          'Authorization': me.token,
          'Accept-Language': 'en',
          'Content-Type': 'application/json; charset=utf-8'
        },
      })
      .then(response => {
        return response.text();
      })
      .then(stream => {
        return JSON.parse(stream);
      })
      .then(categories => {
        return me.refineCategories(categories);
      })
  }

  refineCategories (categories) {
    let map = new Map(categories.map(c => [c.id, c]));

    //setting parent for all categories
    map.forEach(category => {
      category.parent = category.parent && map.get(category.parent);
    });

    //setting childs
    map.forEach(category => {
      category.childs = categories.filter(c => c.parent && c.parent.id === category.id).map(c => c.id);
    });

    return categories;
  }

  // updateCategories : List Category -> Action State
  // updateCategories categories state =
  //   let
  //     idMap = Dict.fromList   <| List.map (\category -> let (Category c) = category in (c.id, category))   categories
  //
  //     findParent mp = flip Maybe.map mp <| \p -> case p of
  //       Left pidx -> mapDefault (Dict.get pidx idMap) p Right
  //       Right _ -> p
  //
  //     categories' = List.map (\(Category c) -> Category { c | parent = findParent c.parent }) categories
  //     dict = Dict.fromList <| List.map (\category ->
  //       let (Category c) = category
  //       in (c.name, Category { c | childs = childs category categories'})) categories'
  //   in
  //     Response <| noFx {state | categories = dict}


  linkPhotos(category, photos) {
    return fetch('//api/v1/category/' + category.id + '/photo', {
        method: 'LINK',
        headers: {
          'Authorization': this.token,
          'Content-Type': 'application/json; charset=utf-8'
        },
        body: JSON.stringify(photos.map(p => p.id))
      });
  }

  unlinkPhotos(category, photos) {
    return fetch('//api/v1/category/' + category.id + '/photo', {
        method: 'UNLINK',
        headers: {
          'Authorization': this.token,
          'Content-Type': 'application/json; charset=utf-8'
        },
        body: JSON.stringify(photos.map(p => p.id))
      });
  }
};

export default CategoryService;
