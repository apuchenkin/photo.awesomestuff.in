import * as React from 'react';
import Category from './Category';

export default class RootCategory extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      expanded: false,
    };
    this.toggleExpand = this.toggleExpand.bind(this);
  }

  toggleExpand() {
    this.setState(state => ({
      expanded: !state.expanded,
    }));
  }

  render() {
    const { category } = this.props;
    const { expanded } = this.state;

    return (
      <li className="item" >
        <button
          className="material-icons"
          onClick={this.toggleExpand}
          disabled={!category.childs.length}
        >{
          expanded
            ? 'keyboard_arrow_up'
            : 'keyboard_arrow_down'
        }</button>
        <Category category={category} />
        {
          expanded && (
            <ul className="childs">
              {category.childs.map(child => (
                <li className="item" key={child.id} >
                  <Category category={child} />
                </li>
              ))}
            </ul>
          )
        }
      </li>
    );
  }
}
