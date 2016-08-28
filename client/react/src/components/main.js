import React from 'react';
import Link from 'react-router/lib/Link';
import Loader from './loader';
import ReactCSSTransitionGroup from 'react-addons-css-transition-group';

import 'perfect-scrollbar/dist/css/perfect-scrollbar.css';

var isBrowser = (typeof window !== 'undefined');
var Ps = isBrowser ? window.Ps || require('perfect-scrollbar') : null;

class Main extends React.Component {
    constructor (props) {
      super(props)

      this.state = {
        isLoading: false,
        class: props.routes[props.routes.length - 1].class
      }
    }

  	componentDidMount() {
  		const
        me = this,
        props = me.props
      ;

      props.route.cmp = this;
      Ps.initialize(me.refs.content);
  	}

    componentDidUpdate() {
      const content = this.refs.content;
      content.scrollTop = 0;
      Ps.update(content);
    }

    componentWillReceiveProps(props) {
      this.setState({
        class: props.routes[props.routes.length - 1].class
      })
    }


    render() {
      const
        state = this.state
      ;

      if(this.props.children) {
        debugger;
      }

      return (
        <div id="main" className={state.class} ref="main">
          <ReactCSSTransitionGroup transitionName="loader" transitionAppearTimeout={200} transitionEnterTimeout={200} transitionLeaveTimeout={200} transitionAppear={false}>
            {this.state.isLoading && <Loader />}
          </ReactCSSTransitionGroup>
          {this.props.header}
          <div className="content" ref="content">
            {this.props.body}
            <footer>
              <Link to="/">photo.awesomestuff.in</Link> | © 2015, Пученкин Артём | <Link to="/about">О сайте</Link> | <Link to="/contacts">Контакты</Link>
            </footer>
          </div>
        </div>
      );
    }
}

// Main.contextTypes = {
//   initialState: React.PropTypes.any.isRequired
// };

export default Main;
