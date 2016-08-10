
const PhotoService = class {

  constructor(token) {
    this.token = token;
  }

  fetchPhotos (category) {
    let me = this;

    return fetch('/api/v1/category/' + category + '/photo', {
        headers: {
          'Authorization': me.token,
          'Content-Type': 'application/json; charset=utf-8'
        },
      })
      .then(response => {
        return response.text();
      })
      .then(stream => {
        return JSON.parse(stream);
      })
  }

  updateParents(photos, parent) {
    return this.fetchPhotos(parent)
      .then(parents => {
        return photos.map(photo => {
          photo.hasParent = parents.find(p => p.id == photo.id);
          return photo;
        });
      });

    // debugger;
    // var category = categories.$findByName(me.category);
    // if (category && category.parent) {
    //   var parent = categories.$findById(category.parent);
    //   me.parentPhotos = parent.photo.$fetch();
    //   me.parentPhotos.$then(function () {
    //     var ids = _.intersection(_.map(photos, 'id'), _.map(me.parentPhotos,'id'));
    //     ids.map(function(i){
    //       _.find(photos, {id: i}).hasParent = true;
    //     });
    //   });
    // }
  }
};

export default PhotoService;
