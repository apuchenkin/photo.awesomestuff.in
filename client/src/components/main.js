import React from 'react';
import { locationShape } from 'react-router/lib/PropTypes';
import withRouter from 'react-router/lib/withRouter';
import withStyles from 'isomorphic-style-loader/lib/withStyles';

import Component from '../lib/PureComponent';
import baseStyle from '../style/style.less';
import style from '../style/main.less';

import Loader from './loader/loader';
import Footer from './footer';

const
  { string, func, element, arrayOf, shape } = React.PropTypes,
  Ps = isBrowser ? require('perfect-scrollbar') : null
  ;

class Main extends Component {

  static propTypes = {
    routes: arrayOf(shape({
      class: string,
      getLangs: func,
    })).isRequired,
    header: element.isRequired,
    body: element.isRequired,
    location: locationShape.isRequired,
  }

  componentDidMount() {
    const me = this;

    Ps.initialize(me.content);
  }

  componentDidUpdate() {
    this.content.scrollTop = 0;
    Ps.update(this.content);
  }

  render() {
    const
      { routes, header, body, location } = this.props,
      route = routes[routes.length - 1],
      langs = route.getLangs && route.getLangs()
      ;

    return (
      <div className={[style.main, style[route.class]].join(' ')}>
        {header}
        <div className={style.content} ref={(c) => { this.content = c; }}>
          {body}
          <Footer
            langs={langs}
            location={location}
          />
        </div>
        <Loader key="loader" />
      </div>
    );
  }
}

export default withStyles(style, baseStyle)(
  withRouter(Main)
);
