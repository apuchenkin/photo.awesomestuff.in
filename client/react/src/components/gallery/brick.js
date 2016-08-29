import React from 'react';
import config from '../../config.json';

const { object } = React.PropTypes;

export default class Brick extends React.Component {
	static propTypes = {
		photo: object.isRequired
	}

	render() {
    let
			photo = this.props.photo,
			{ w, h, ratio } = photo,
      inc = ratio >= 1 ? ratio : 1 / ratio,
			[m1,m2] = w < h ? [Math.ceil(w * inc), h] : [Math.ceil(h * inc), w],
			s = Math.max(m1, m2),
			filename = photo.src.split('/').pop(),
			src = [config.apiEndpoint + config.apiPrefix, 'hs/photo', photo.id, s, s, filename].join('/')
		;

		return (
			<div className="brick" style={{width: w + 'px', height: h + 'px', backgroundImage: `url(${src})`}} />
		);
	}
}
